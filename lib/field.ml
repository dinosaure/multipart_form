type 'a t =
  | Content_type : Content_type.t t
  | Content_encoding : Content_encoding.t t
  | Content_disposition : Content_disposition.t t
  | Field : Field_name.t -> Unstrctrd.t t

type witness = Witness : 'a t -> witness

type field = Field : Field_name.t * 'a t * 'a -> field

let ( <.> ) f g x = f (g x)

let of_field_name : Field_name.t -> witness =
 fun field_name ->
  match String.lowercase_ascii (field_name :> string) with
  | "content-type" -> Witness Content_type
  | "content-transfer-encoding" -> Witness Content_encoding
  | "content-disposition" -> Witness Content_disposition
  | v -> Witness (Field v)

let parser : type a. a t -> a Angstrom.t = function
  | Content_type -> Content_type.Decoder.content
  | Content_encoding -> Content_encoding.Decoder.mechanism
  | Content_disposition -> assert false
  | Field _ ->
      let buf = Bytes.create 0x7f in
      Unstrctrd_parser.unstrctrd buf

module Decoder = struct
  open Angstrom

  let field ?g field_name =
    let buf = Bytes.create 0x7f in
    (* XXX(dinosaure): fast allocation. *)
    Unstrctrd_parser.unstrctrd buf >>= fun v ->
    let (Witness w) =
      match Option.bind g (Field_name.Map.find_opt field_name) with
      | None -> of_field_name field_name
      | Some w -> w in
    let parser = parser w in
    let res =
      let open Rresult in
      Unstrctrd.without_comments v
      >>| Unstrctrd.fold_fws
      >>| Unstrctrd.to_utf_8_string
      (* XXX(dinosaure): normalized value can have trailing whitespace
       * such as "value (comment)" returns "value ". Given parser can
       * ignore it (and it does not consume all inputs finally). *)
      >>= (R.reword_error R.msg
          <.> (parse_string ~consume:Consume.Prefix) parser)
      >>| fun v -> Field (field_name, w, v) in
    match res with
    | Ok v -> return v
    | Error _ -> return (Field (field_name, Field field_name, v))
end
