open Tezos_error_monad.Error_monad
module Context_hash = Tezos_crypto.Context_hash
module Hash = Store_hash

type hash = [ `Blob of Store.hash | `Node of Store.hash ]

module Data_encoding = struct
  type command = Node of (string * hash) list | Blob of string | End

  (* Command encoding. *)

  let hash_encoding : hash Data_encoding.t =
    let open Data_encoding in
    let kind_encoding = string_enum [ ("node", `Node); ("blob", `Blob) ] in
    conv
      (function
        | `Blob h -> (`Blob, Hash.to_bytes h)
        | `Node h -> (`Node, Hash.to_bytes h))
      (function
        | `Blob, h -> `Blob (Hash.of_bytes h)
        | `Node, h -> `Node (Hash.of_bytes h))
      (obj2 (req "kind" kind_encoding) (req "value" bytes))

  let blob_encoding =
    let open Data_encoding in
    case ~title:"blob"
      (Tag (Char.code 'b'))
      string
      (function Blob string -> Some string | _ -> None)
      (function string -> Blob string)

  let node_encoding =
    let open Data_encoding in
    case ~title:"node"
      (Tag (Char.code 'd'))
      (list (obj2 (req "name" string) (req "hash" hash_encoding)))
      (function Node x -> Some x | _ -> None)
      (function x -> Node x)

  let end_encoding =
    let open Data_encoding in
    case ~title:"end"
      (Tag (Char.code 'e'))
      empty
      (function End -> Some () | _ -> None)
      (fun () -> End)

  let command_encoding =
    Data_encoding.union ~tag_size:`Uint8
      [ blob_encoding; node_encoding; end_encoding ]

  (* IO toolkit. *)

  let rec read_string rbuf ~len =
    let fd, buf, ofs, total = !rbuf in
    if Bytes.length buf - ofs < len then (
      let blen = Bytes.length buf - ofs in
      let neu = Bytes.create (blen + 1_000_000) in
      Bytes.blit buf ofs neu 0 blen;
      Lwt_unix.read fd neu blen 1_000_000 >>= fun bread ->
      total := !total + bread;
      if bread = 0 then assert false
      else
        let neu =
          if bread <> 1_000_000 then Bytes.sub neu 0 (blen + bread) else neu
        in
        rbuf := (fd, neu, 0, total);
        read_string rbuf ~len )
    else
      let res = Bytes.sub_string buf ofs len in
      rbuf := (fd, buf, ofs + len, total);
      return res

  let read_mbytes rbuf b =
    read_string rbuf ~len:(Bytes.length b) >>=? fun string ->
    Bytes.blit_string string 0 b 0 (Bytes.length b);
    return ()

  let set_int64 buf i =
    let b = Bytes.create 8 in
    EndianBytes.BigEndian.set_int64 b 0 i;
    Buffer.add_bytes buf b

  let get_int64 rbuf =
    read_string ~len:8 rbuf >>=? fun s ->
    return @@ EndianString.BigEndian.get_int64 s 0

  let set_mbytes buf b =
    set_int64 buf (Int64.of_int (Bytes.length b));
    Buffer.add_bytes buf b

  let get_mbytes rbuf =
    get_int64 rbuf >>|? Int64.to_int >>=? fun l ->
    let b = Bytes.create l in
    read_mbytes rbuf b >>=? fun () -> return b

  (* Getter and setters *)

  let get_command rbuf =
    get_mbytes rbuf >>|? fun bytes ->
    Data_encoding.Binary.of_bytes_exn command_encoding bytes

  let set_node buf contents =
    let bytes =
      Data_encoding.Binary.to_bytes_exn command_encoding (Node contents)
    in
    set_mbytes buf bytes

  let set_blob buf data =
    let bytes =
      Data_encoding.Binary.to_bytes_exn command_encoding (Blob data)
    in
    set_mbytes buf bytes
end
