open! Lwt.Infix

let root =
  "/home/craigfe/data/tezos/tezos-node_carthagenet_2020-09-20_unknown-key/context"

let output = "/tmp/data"

let ( let* ) = Lwt.bind

let target = "CoVHqwb5Push49CDYWpCw8dksQ6xKqUS2ChEyuSNgC3arvwwGi4m"

let main () =
  Printf.printf "%s (target)\n\n" target;
  let count = ref 0 in
  Store_index.v ~log_size:0 root
  |> Store_index.iter (fun k v ->
         let hash =
           Fmt.to_to_string Tezos_crypto.Context_hash.pp
             (Store_hash.to_context_hash k)
         in
         let pp_value ppf (a, b, c) =
           Format.fprintf ppf "(%Ld, %d, %c)" a b c
         in
         if hash = target then (
           Format.printf
             "DONE!\n\n\
              Found the context hash `%s' at index `%d' in the iteration over \
              the index. The true key is `%s' and the true value is `%a'\n\n\
              %!"
             target !count
             (Store_hash.to_hex_string k)
             pp_value v;
           raise (Failure "DEBUG") );
         incr count;
         if !count mod 100_000 = 0 then
           Printf.printf "Not found @ %d entries\n%!" !count);
  Printf.printf "Didn't find anything%!"

let () = main ()

(* let main () =
  let* repo =
    Store.Repo.v (Irmin_pack.config ~readonly:true ~lru_size:0 root)
  in
  ( match Store.integrity_check ~ppf:Fmt.stderr ~auto_repair:false repo with
  | Ok (`Fixed i) -> Fmt.epr "Ok (Fixed %d)%!\n" i
  | Ok `No_error -> Fmt.epr "Ok No_error%!\n"
  | Error (`Cannot_fix s) -> Fmt.epr "Error (Cannot_fix %s)%!\n" s
  | Error (`Corrupted i) -> Fmt.epr "Error (Corrupted %d)%!\n" i );
   Lwt.return_unit *)

(* let () = Lwt_main.run (main ()) *)
