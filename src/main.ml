open Lwt.Infix

let root =
  "/home/craigfe/data/tezos/stores/full_store_BLAktkWruUqXNgHAiR7kLh4dMP96mGmQANDGagdHAsTXfqgvfiR_933914/context"

let store_hash_hex =
  "2ef3daa18c79b75446b8608885afde542404c63f9ca5f4029017898a48f6cb7d"

let output = "/tmp/data"

let ( let* ) = Lwt.bind

let main () =
  let* repo = Store.Repo.v (Irmin_pack.config ~readonly:true root) in
  let hash = Store_hash.Hash.of_hex_string store_hash_hex in
  let* tree = Store.Tree.of_hash repo hash >|= Option.get in
  let* fd =
    Lwt_unix.openfile output Lwt_unix.[ O_WRONLY; O_CREAT; O_TRUNC ] 0o666
  in
  Tree_fold.dump tree fd

let () = Lwt_main.run (main ())
