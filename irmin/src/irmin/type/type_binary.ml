(*
 * Copyright (c) 2016-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Type_core

module B = struct
  external get_16 : string -> int -> int = "%caml_string_get16"

  external get_32 : string -> int -> int32 = "%caml_string_get32"

  external get_64 : string -> int -> int64 = "%caml_string_get64"

  external set_16 : Bytes.t -> int -> int -> unit = "%caml_string_set16u"

  external set_32 : Bytes.t -> int -> int32 -> unit = "%caml_string_set32u"

  external set_64 : Bytes.t -> int -> int64 -> unit = "%caml_string_set64u"

  external swap16 : int -> int = "%bswap16"

  external swap32 : int32 -> int32 = "%bswap_int32"

  external swap64 : int64 -> int64 = "%bswap_int64"

  let get_uint16 s off =
    if not Sys.big_endian then swap16 (get_16 s off) else get_16 s off

  let get_uint32 s off =
    if not Sys.big_endian then swap32 (get_32 s off) else get_32 s off

  let get_uint64 s off =
    if not Sys.big_endian then swap64 (get_64 s off) else get_64 s off

  let set_uint16 s off v =
    if not Sys.big_endian then set_16 s off (swap16 v) else set_16 s off v

  let set_uint32 s off v =
    if not Sys.big_endian then set_32 s off (swap32 v) else set_32 s off v

  let set_uint64 s off v =
    if not Sys.big_endian then set_64 s off (swap64 v) else set_64 s off v
end

module Encode = struct
  let unit () _k = ()

  let add_bytes b k = k (Bytes.to_string b)

  let add_string s k = k s

  let char c = add_bytes (Bytes.make 1 c)

  let int8 i = char (Char.chr i)

  let int16 i =
    let b = Bytes.create 2 in
    B.set_uint16 b 0 i;
    add_bytes b

  let int32 i =
    let b = Bytes.create 4 in
    B.set_uint32 b 0 i;
    add_bytes b

  let int64 i =
    let b = Bytes.create 8 in
    B.set_uint64 b 0 i;
    add_bytes b

  let float f = int64 (Int64.bits_of_float f)

  let bool b = char (if b then '\255' else '\000')

  let int i k =
    let rec aux n =
      if n >= 0 && n < 128 then int8 n k
      else
        let out = 128 + (n land 127) in
        int8 out k;
        aux (n lsr 7)
    in
    aux i

  let len n i =
    match n with
    | `Int -> int i
    | `Int8 -> int8 i
    | `Int16 -> int16 i
    | `Int32 -> int32 (Int32.of_int i)
    | `Int64 -> int64 (Int64.of_int i)
    | `Fixed _ -> unit ()

  let string ?(headers = true) =
    match headers with
    | false ->
        fun _ ->
          (* we might want to check invariants such as
             n=`Fixed len <=> len(s) = len *)
          add_string
    | true ->
        fun n s k ->
          let i = String.length s in
          len n i k;
          add_string s k

  let bytes ?(headers = true) =
    match headers with
    | false -> fun _ -> add_bytes
    | true ->
        fun n s k ->
          let i = Bytes.length s in
          len n i k;
          add_bytes s k

  let list l n x k =
    len n (List.length x) k;
    List.iter (fun e -> l e k) x

  let array l n x k =
    len n (Array.length x) k;
    Array.iter (fun e -> l e k) x

  let pair a b (x, y) k =
    a x k;
    b y k

  let triple a b c (x, y, z) k =
    a x k;
    b y k;
    c z k

  let option o v k =
    match v with
    | None -> char '\000' k
    | Some x ->
        char '\255' k;
        o x k

  let rec t : type a. a t -> a encode_bin =
   fun ty ~headers ->
    match ty with
    | Self s -> t ~headers s.self_fix
    | Custom c -> c.encode_bin ~headers
    | Map b -> map ~headers b
    | Prim t -> prim ~headers t
    | List l -> list (t ~headers:true l.v) l.len
    | Array a -> array (t ~headers:true a.v) a.len
    | Tuple t -> tuple ~headers t
    | Option x -> option (t ~headers:true x)
    | Record r -> record ~headers r
    | Variant v -> variant ~headers v
    | Var v -> raise (Unbound_type_variable v)

  and tuple : type a. a tuple -> a encode_bin =
   fun ty ~headers:_ ->
    match ty with
    | Pair (x, y) -> pair (t ~headers:true x) (t ~headers:true y)
    | Triple (x, y, z) ->
        triple (t ~headers:true x) (t ~headers:true y) (t ~headers:true z)

  and map : type a b. (a, b) map -> b encode_bin =
   fun { x; g; _ } ~headers u k -> t ~headers x (g u) k

  and prim : type a. a prim -> a encode_bin =
   fun ty ~headers ->
    match ty with
    | Unit -> unit
    | Bool -> bool
    | Char -> char
    | Int -> int
    | Int32 -> int32
    | Int64 -> int64
    | Float -> float
    | String n -> string ~headers n
    | Bytes n -> bytes ~headers n

  and record : type a. a record -> a encode_bin =
   fun r ~headers:_ x k ->
    let fields = fields r in
    List.iter (fun (Field f) -> t ~headers:true f.ftype (f.fget x) k) fields

  and variant : type a. a variant -> a encode_bin =
   fun v ~headers:_ x k -> case_v ~headers:true (v.vget x) k

  and case_v : type a. a case_v encode_bin =
   fun ~headers:_ c k ->
    match c with
    | CV0 c -> int c.ctag0 k
    | CV1 (c, v) ->
        int c.ctag1 k;
        t ~headers:true c.ctype1 v k
end

module Decode = struct
  type 'a res = int * 'a

  let unit _ ofs = (ofs, ())

  let char buf ofs = (ofs + 1, buf.[ofs])

  let int8 buf ofs =
    let ofs, c = char buf ofs in
    (ofs, Char.code c)

  let int16 buf ofs = (ofs + 2, B.get_uint16 buf ofs)

  let int32 buf ofs = (ofs + 4, B.get_uint32 buf ofs)

  let int64 buf ofs = (ofs + 8, B.get_uint64 buf ofs)

  let bool buf ofs =
    let ofs, c = char buf ofs in
    match c with '\000' -> (ofs, false) | _ -> (ofs, true)

  let float buf ofs =
    let ofs, f = int64 buf ofs in
    (ofs, Int64.float_of_bits f)

  let int buf ofs =
    let rec aux n p ofs =
      let ofs, i = int8 buf ofs in
      let n = n + ((i land 127) lsl (p * 7)) in
      if i >= 0 && i < 128 then (ofs, n) else aux n (p + 1) ofs
    in
    aux 0 0 ofs

  let len buf ofs = function
    | `Int -> int buf ofs
    | `Int8 -> int8 buf ofs
    | `Int16 -> int16 buf ofs
    | `Int32 ->
        let ofs, i = int32 buf ofs in
        (ofs, Int32.to_int i)
    | `Int64 ->
        let ofs, i = int64 buf ofs in
        (ofs, Int64.to_int i)
    | `Fixed n -> (ofs, n)

  let string =
    let sub len buf ofs =
      if ofs = 0 && len = String.length buf then (len, buf)
      else
        let str = Bytes.create len in
        String.blit buf ofs str 0 len;
        (ofs + len, Bytes.unsafe_to_string str)
    in
    fun ~headers n ->
      match (headers, n) with
      | _, `Fixed len -> sub len
      | true, _ ->
          fun buf ofs ->
            let ofs, len = len buf ofs n in
            sub len buf ofs
      | false, _ -> fun buf ofs -> sub (String.length buf - ofs) buf ofs

  let bytes =
    let sub len buf ofs =
      if ofs = 0 && len = String.length buf then (len, Bytes.of_string buf)
      else
        let str = Bytes.create len in
        String.blit buf ofs str 0 len;
        (ofs + len, str)
    in
    fun ~headers n ->
      match (headers, n) with
      | _, `Fixed len -> sub len
      | true, _ ->
          fun buf ofs ->
            let ofs, len = len buf ofs n in
            sub len buf ofs
      | false, _ -> fun buf ofs -> sub (String.length buf - ofs) buf ofs

  let list l n buf ofs =
    let ofs, len = len buf ofs n in
    let rec aux acc ofs = function
      | 0 -> (ofs, List.rev acc)
      | n ->
          let ofs, x = l buf ofs in
          aux (x :: acc) ofs (n - 1)
    in
    aux [] ofs len

  let array l len buf ofs =
    let ofs, l = list l len buf ofs in
    (ofs, Array.of_list l)

  let pair a b buf ofs =
    let ofs, a = a buf ofs in
    let ofs, b = b buf ofs in
    (ofs, (a, b))

  let triple a b c buf ofs =
    let ofs, a = a buf ofs in
    let ofs, b = b buf ofs in
    let ofs, c = c buf ofs in
    (ofs, (a, b, c))

  let option : type a. a decode_bin -> a option decode_bin =
   fun o ~headers:_ buf ofs ->
    let ofs, c = char buf ofs in
    match c with
    | '\000' -> (ofs, None)
    | _ ->
        let ofs, x = o ~headers:true buf ofs in
        (ofs, Some x)

  let rec t : type a. a t -> a decode_bin =
   fun ty ~headers ->
    match ty with
    | Self s -> t ~headers s.self_fix
    | Custom c -> c.decode_bin ~headers
    | Map b -> map ~headers b
    | Prim t -> prim ~headers t
    | List l -> list (t ~headers:true l.v) l.len
    | Array a -> array (t ~headers:true a.v) a.len
    | Tuple t -> tuple ~headers t
    | Option x -> option ~headers (t x)
    | Record r -> record ~headers r
    | Variant v -> variant ~headers v
    | Var v -> raise (Unbound_type_variable v)

  and tuple : type a. a tuple -> a decode_bin =
   fun ty ~headers:_ ->
    match ty with
    | Pair (x, y) -> pair (t ~headers:true x) (t ~headers:true y)
    | Triple (x, y, z) ->
        triple (t ~headers:true x) (t ~headers:true y) (t ~headers:true z)

  and map : type a b. (a, b) map -> b decode_bin =
   fun { x; f; _ } ~headers buf ofs ->
    let ofs, x = t ~headers x buf ofs in
    (ofs, f x)

  and prim : type a. a prim -> a decode_bin =
   fun ty ~headers ->
    match ty with
    | Unit -> unit
    | Bool -> bool
    | Char -> char
    | Int -> int
    | Int32 -> int32
    | Int64 -> int64
    | Float -> float
    | String n -> string ~headers n
    | Bytes n -> bytes ~headers n

  and record : type a. a record -> a decode_bin =
   fun r ~headers:_ buf ofs ->
    match r.rfields with
    | Fields (fs, c) ->
        let rec aux : type b. int -> b -> (a, b) fields -> a res =
         fun ofs f -> function
          | F0 -> (ofs, f)
          | F1 (h, t) ->
              let ofs, x = field ~headers:true h buf ofs in
              aux ofs (f x) t
        in
        aux ofs c fs

  and field : type a b. (a, b) field -> b decode_bin = fun f -> t f.ftype

  and variant : type a. a variant -> a decode_bin =
   fun v ~headers:_ buf ofs ->
    let ofs, i = int buf ofs in
    case ~headers:true v.vcases.(i) buf ofs

  and case : type a. a a_case -> a decode_bin =
   fun c ~headers:_ buf ofs ->
    match c with
    | C0 c -> (ofs, c.c0)
    | C1 c ->
        let ofs, x = t ~headers:true c.ctype1 buf ofs in
        (ofs, c.c1 x)
end

let encode_bin = Encode.t

let decode_bin = Decode.t

let to_bin size_of encode_bin x =
  let seq = encode_bin ~headers:false x in
  let len = match size_of ~headers:false x with None -> 1024 | Some n -> n in
  let buf = Buffer.create len in
  seq (Buffer.add_string buf);
  Buffer.contents buf

let to_bin_string t =
  let rec aux : type a. a t -> a -> string =
   fun t ->
    match t with
    | Self s -> aux s.self_fix
    | Map m -> fun x -> aux m.x (m.g x)
    | Prim (String _) -> fun x -> x
    | Prim (Bytes _) -> Bytes.to_string
    | Custom c -> to_bin c.size_of c.encode_bin
    | _ -> to_bin (Type_size.t t) (Encode.t t)
  in
  aux t

let map_result f = function Ok x -> Ok (f x) | Error _ as e -> e

let of_bin decode_bin x =
  let last, v = decode_bin ~headers:false x 0 in
  assert (last = String.length x);
  Ok v

let of_bin_string t =
  let rec aux : type a. a t -> string -> (a, [ `Msg of string ]) result =
   fun t ->
    match t with
    | Self s -> aux s.self_fix
    | Map l -> fun x -> aux l.x x |> map_result l.f
    | Prim (String _) -> fun x -> Ok x
    | Prim (Bytes _) -> fun x -> Ok (Bytes.of_string x)
    | Custom c -> of_bin c.decode_bin
    | _ -> of_bin (Decode.t t)
  in
  fun x -> try aux t x with Invalid_argument e -> Error (`Msg e)
