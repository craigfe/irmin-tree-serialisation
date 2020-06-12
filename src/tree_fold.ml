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

module Node = Store.Private.Node
module G = Irmin.Private.Node.Graph (Node)

(* Folding through a node *)
let fold_tree_path ~(maybe_flush : unit -> unit Lwt.t) ~buf repo hash =
  let n = Store.Private.Repo.node_t repo in
  G.iter n ~min:[] ~max:[ hash ]
    ~node:(fun _ value ->
      let sub_keys =
        List.rev_map
          (fun (name, value) ->
            match value with
            | `Node hash -> (name, `Node hash)
            | `Contents (hash, _) -> (name, `Blob hash))
          value
      in
      set_node buf sub_keys;
      maybe_flush ())
    ~contents:(fun hash ->
      Store.Private.Contents.find (Store.Private.Repo.contents_t repo) hash
      >>= function
      | None -> Lwt.return ()
      | Some data ->
          set_blob buf data;
          maybe_flush ())
    ()

let dump repo hash fd =
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
  fold_tree_path ~maybe_flush ~buf repo hash
