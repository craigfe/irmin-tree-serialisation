module Context_hash = Tezos_crypto.Context_hash
module Error_monad = Tezos_error_monad.Error_monad

module Hash : sig
  include Irmin.Hash.S

  val to_context_hash : t -> Context_hash.t

  val of_context_hash : Context_hash.t -> t

  val of_hex_string : string -> t

  val truncate : int -> t -> string
  (** Truncate a hash to [n]-many bytes of entropy. *)
end = struct
  module H = Digestif.Make_BLAKE2B (struct
    let digest_size = 32
  end)

  type t = H.t

  let of_hex_string = H.of_hex

  let of_context_hash s = H.of_raw_string (Context_hash.to_string s)

  let to_context_hash h = Context_hash.of_string_exn (H.to_raw_string h)

  let pp ppf t = Context_hash.pp ppf (to_context_hash t)

  let truncate n h =
    assert (n > 0 && n < 33);
    let raw = H.to_raw_string h in
    String.sub raw 0 n

  let of_string x =
    match Context_hash.of_b58check x with
    | Ok x -> Ok (of_context_hash x)
    | Error err ->
        Error
          (`Msg
            (Format.asprintf "Failed to read b58check_encoding data: %a"
               Error_monad.pp_print_error err))

  let short_hash t = Irmin.Type.(short_hash string (H.to_raw_string t))

  let t : t Irmin.Type.t =
    Irmin.Type.map ~cli:(pp, of_string)
      Irmin.Type.(string_of (`Fixed H.digest_size))
      ~short_hash H.of_raw_string H.to_raw_string

  let hash_size = H.digest_size

  let hash = H.digesti_string
end

include Hash
