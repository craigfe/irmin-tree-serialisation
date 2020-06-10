open Lwt.Infix
open Serialise.Data_encoding

let ( let+ ) x f = Lwt.map f x

let ( let* ) = Lwt.bind

module Utils = struct
  let hide_progress_line s =
    let len = String.length s in
    if len > 0 then Printf.eprintf "\r%*s\r" len ""

  let display_progress ?(refresh_rate = (1, 1)) msgf =
    if Unix.isatty Unix.stderr then
      let index, rate = refresh_rate in
      if index mod rate == 0 then
        msgf
          (Format.kasprintf (fun msg ->
               hide_progress_line msg;
               Format.eprintf "%s%!" msg))

  let display_progress_end () =
    if Unix.isatty Unix.stderr then Format.eprintf "@."

  let write_string ?(pos = 0) ?len descr buf =
    let len = match len with None -> String.length buf - pos | Some l -> l in
    let rec inner pos len =
      if len = 0 then Lwt.return_unit
      else
        Lwt_unix.write_string descr buf pos len >>= function
        | 0 -> Lwt.fail End_of_file
        (* other endpoint cleanly closed its connection *)
        | nb_written -> inner (pos + nb_written) (len - nb_written)
    in
    inner pos len
end

module I = struct
  let tree_list tree =
    let+ ret = Store.Tree.list tree [] in
    Fmt.epr "\ntree_list: %a\n" Fmt.(Dump.list (using fst string)) ret;
    ret

  let sub_tree tree key =
    Store.Tree.get_tree tree key >|= fun subtree ->
    Store.Tree.clear tree;
    subtree

  let tree_hash = function
    | `Node _ as tree -> `Node (Store.Tree.hash tree)
    | `Contents (b, _) -> `Blob (Store.Contents.hash b)

  let tree_content tree = Store.Tree.find tree []
end

let heap_log, statmemprof_log, event_log =
  Random.self_init ();
  let uid = Random.int64 Int64.max_int in
  let tmp_dir = Fmt.str "/tmp/stats-%Ld" uid in
  let statfmt name =
    tmp_dir ^ "/" ^ name |> open_out |> Format.formatter_of_out_channel
  in
  Unix.mkdir tmp_dir 0o755;
  let heap = statfmt "heap"
  and statmemprof = statfmt "statmemprof"
  and events = statfmt "events" in

  Fmt.pf heap "sys time,total visited,minor words,major words\n";
  Fmt.pf events "total_visited,type,systime_start,systime_end\n";

  Fmt.pr "This run has id `%Ld'\n" uid;
  Fmt.pr "Stats streaming to `%s'\n%!" tmp_dir;
  (heap, statmemprof, events)

(* Folding through a node *)
let fold_tree_path ~(written : int ref) ~(maybe_flush : unit -> unit Lwt.t) ~buf
    tree =
  (* Noting the visited hashes *)
  let visited_hash = Hashtbl.create 1000 in
  let visited h = Hashtbl.mem visited_hash h in
  let set_visit =
    let total_visited = ref 0 in
    fun h ->
      Utils.display_progress ~refresh_rate:(!total_visited, 1_000) (fun m ->
          m "Context: %dK elements, %dMiB written%!" (!total_visited / 1_000)
            (!written / 1_048_576));

      ( if !total_visited mod 10_000 = 0 then
        let Gc.{ major_words; minor_words; _ } = Gc.quick_stat () in
        (* let hashtbl_words = Obj.reachable_words (Obj.repr visited_hash) in *)
        Fmt.pf heap_log "%f,%d,%f,%f\n%!" (Sys.time ()) !total_visited
          minor_words major_words
        (* if !total_visited = 3_000_000 then memprof_active := true ; *) );

      if !total_visited mod 100_000 = 0 then (
        let time_started = Sys.time () in
        Gc.minor ();
        let time_ended = Sys.time () in
        Fmt.pf event_log "%d,minor_collect,%f,%f\n%!" !total_visited
          time_started time_ended );

      incr total_visited;
      Hashtbl.add visited_hash h ();
      ()
  in
  let rec fold_tree_path tree ret =
    let* keys = I.tree_list tree in
    let keys = List.sort (fun (a, _) (b, _) -> String.compare a b) keys in

    let* sub_keys =
      keys
      |> Lwt_list.fold_left_s
           (fun acc (name, kind) ->
             let* sub_tree = I.sub_tree tree [ name ] in
             let hash = I.tree_hash sub_tree in
             let acc = (name, hash) :: acc in
             if visited hash then Lwt.return acc
             else (
               set_visit hash;
               (* There cannot be a cycle *)
               match kind with
               | `Node -> (fold_tree_path [@ocaml.tailcall]) sub_tree acc
               | `Contents -> (
                   I.tree_content sub_tree >>= function
                   | None -> assert false
                   | Some data ->
                       set_blob buf data;
                       maybe_flush () >|= fun () -> acc ) ))
           []
    in

    set_node buf sub_keys;
    maybe_flush () >|= fun () -> ret
  in
  fold_tree_path tree [] >|= function [] -> () | _ -> assert false

let dump tree fd =
  let buf = Buffer.create 1_000_000 in
  let written = ref 0 in
  let flush () =
    let contents = Buffer.contents buf in
    Buffer.reset buf;
    written := !written + String.length contents;
    Utils.write_string fd contents
  in
  let maybe_flush () =
    if (* true *) Buffer.length buf > 1_000_000 then flush ()
    else Lwt.return_unit
  in
  fold_tree_path ~written ~maybe_flush ~buf tree
