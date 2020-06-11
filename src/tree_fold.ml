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
  let tree_list tree = Store.Tree.list tree []

  let sub_tree tree key =
    Store.Tree.get_tree tree key >|= fun subtree -> subtree

  let tree_hash = function
    | `Node tree -> `Node (Store.Tree.hash tree)
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

  Fmt.pf heap "sys time,total visited,minor words,major words,live\n";
  Fmt.pf events "total_visited,type,systime_start,systime_end\n";

  Fmt.pr "This run has id `%Ld'\n" uid;
  Fmt.pr "Stats streaming to `%s'\n%!" tmp_dir;
  (heap, statmemprof, events)

module Tbl = struct
  include Hashset.Make (struct
    type t = string

    external get_64 : string -> int -> int64 = "%caml_string_get64u"

    let hash x = Int64.to_int (get_64 x 0)

    let equal x y = String.equal x y
  end)

  let of_hash h = String.sub (Irmin.Type.to_bin_string Store.Hash.t h) 0 5

  let mem t k = mem t (of_hash k)

  let add t k = add t (of_hash k)
end

module G = Irmin.Private.Node.Graph (Store.Private.Node)

(* Folding through a node *)
let fold_tree_path ~(written : int ref) ~(maybe_flush : unit -> unit Lwt.t) ~buf
    tree =
  (* Noting the visited hashes *)
  let visited_hash = Tbl.create 100_000 in
  let visited h = Tbl.mem visited_hash h in
  let set_visit =
    let total_visited = ref 0 in
    fun h ->
      Utils.display_progress ~refresh_rate:(!total_visited, 1_000) (fun m ->
          m "Context: %dK elements, %dMiB written%!" (!total_visited / 1_000)
            (!written / 1_048_576));

      incr total_visited;
      Tbl.add visited_hash h;
      ()
  in
  let rec iter_tree tree =
    let* keys = I.tree_list tree in
    (*    let keys = List.sort (fun (a, _) (b, _) -> String.compare a b) keys in *)
    let* keys =
      Lwt_list.fold_left_s
        (fun acc (name, kind) ->
          let+ sub_tree = I.sub_tree tree [ name ] in
          let hash = Store.Tree.hash sub_tree in
          (name, kind, sub_tree, hash) :: acc)
        [] keys
    in
    Store.Tree.clear tree;
    let* () =
      Lwt_list.iter_s
        (fun (_, kind, sub_tree, hash) ->
          if visited hash then Lwt.return ()
          else (
            set_visit hash;
            (* There cannot be a cycle *)
            match kind with
            | `Node -> (iter_tree [@ocaml.tailcall]) sub_tree
            | `Contents -> (
                I.tree_content sub_tree >>= function
                | None -> assert false
                | Some data ->
                    set_blob buf data;
                    maybe_flush () ) ))
        keys
    in
    let sub_keys =
      List.map
        (fun (name, kind, _, hash) ->
          match kind with
          | `Node -> (name, `Node hash)
          | `Contents -> (name, `Blob hash))
        keys
    in
    set_node buf sub_keys;
    maybe_flush ()
  in
  iter_tree tree

let dump tree fd =
  let buf = Buffer.create 10_000_000 in
  let written = ref 0 in
  let flush () =
    let contents = Buffer.contents buf in
    Buffer.clear buf;
    written := !written + String.length contents;
    Utils.write_string fd contents
  in
  let maybe_flush () =
    if (* true *) Buffer.length buf > 1_000_000 then flush ()
    else Lwt.return_unit
  in
  fold_tree_path ~written ~maybe_flush ~buf tree
