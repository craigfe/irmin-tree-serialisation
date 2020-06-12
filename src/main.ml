let root = "data/context"

let store_hash_hex =
  "2ef3daa18c79b75446b8608885afde542404c63f9ca5f4029017898a48f6cb7d"

let output = "/tmp/data"

let ( let* ) = Lwt.bind

let main () =
  let* repo = Store.Repo.v (Irmin_pack.config ~readonly:false root) in
  let hash = Store_hash.Hash.of_hex_string store_hash_hex in
  let* fd =
    Lwt_unix.openfile output Lwt_unix.[ O_WRONLY; O_CREAT; O_TRUNC ] 0o666
  in
  Tree_fold.dump repo hash fd

let reporter ?(prefix = "") () =
  let report src level ~over k msgf =
    let k _ =
      over ();
      k ()
    in
    let ppf = match level with Logs.App -> Fmt.stdout | _ -> Fmt.stderr in
    let with_stamp h _tags k fmt =
      let dt = Unix.gettimeofday () in
      Fmt.kpf k ppf
        ("%s%+04.0fus %a %a @[" ^^ fmt ^^ "@]@.")
        prefix dt
        Fmt.(styled `Magenta string)
        (Logs.Src.name src) Logs_fmt.pp_header (level, h)
    in
    msgf @@ fun ?header ?tags fmt -> with_stamp header tags k fmt
  in
  { Logs.report }

(*
let () =
  Logs.set_level (Some Logs.Debug);
  Logs.set_reporter (reporter ())
 *)

let () = Lwt_main.run (main ())
