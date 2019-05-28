type discrete = [`Text | `Image | `Audio | `Video | `Application]
type composite = [`Multipart]
type extension = [`Ietf_token of string | `X_token of string]
type ty = [discrete | composite | extension]
type subty = [`Ietf_token of string | `Iana_token of string | `X_token of string]
type value = [`String of string | `Token of string]
type disposition_type = [`Inline | `Attachment | `Ietf_token of string | `X_token of string]
type date = unit

type content_encoding =
  [ `Bit7
  | `Bit8
  | `Binary
  | `Quoted_printable
  | `Base64
  | `Ietf_token of string
  | `X_token of string ]

type unstructured =
  [ `Text of string
  | `CR of int
  | `LF of int
  | `CRLF
  | `WSP of string
  | `Encoded of Rfc2047.encoded_word ]
  list

type content_type = {ty: ty; subty: subty; parameters: (string * value) list}
type content_disposition =
  { ty: disposition_type
  ; filename: string option
  ; creation: date option
  ; modification: date option
  ; read: date option
  ; size: int option
  ; parameters: (string * value) list}

type 'a t =
  | Type : content_type t
  | Encoding : content_encoding t
  | Disposition : content_disposition t
  | Field : Field_name.t -> unstructured t

type 'a v =
  | Type : content_type v
  | Encoding : content_encoding v
  | Disposition : content_disposition v
  | Unstructured : unstructured v

type field_name = Field_name : 'a t -> field_name
type field_value = Field_value : 'a v -> field_value
type field = Field : 'a t * 'a -> field
