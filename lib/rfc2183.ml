open Field

type parameter =
  | Filename of value
  | Creation of unit
  | Modification of unit
  | Read of unit
  | Size of int
  | Parameter of (string * value)

open Angstrom

(* From RFC 2045

        tspecials :=  "(" / ")" / "<" / ">" / "@" /
                      "," / ";" / ":" / "\" / <">
                      "/" / "[" / "]" / "?" / "="
                      ; Must be in quoted-string,
                      ; to use within parameter values

      Note that the definition of "tspecials" is the same as the RFC 822
      definition of "specials" with the addition of the three characters
      "/", "?", and "=", and the removal of ".".
*)
let is_tspecials = function
  | '(' | ')' | '<' | '>' | '@' | ',' | ';' | ':' | '\\' | '"' | '/' | '['
   |']' | '?' | '=' ->
      true
  | _ -> false

let invalid_token token = Fmt.kstrf fail "invalid token: %s" token
let nothing_to_do = Fmt.kstrf fail "nothing to do"

(* / *)

let is_ctl = function '\000' .. '\031' | '\127' -> true | _ -> false
let is_space = ( = ) ' '

(* From RFC 2045

        token := 1*<any (US-ASCII) CHAR except SPACE, CTLs,
                    or tspecials>
*)
let is_ascii = function '\000' .. '\127' -> true | _ -> false
let is_token c = (is_ascii c) && (not (is_tspecials c)) && (not (is_ctl c)) && (not (is_space c))
let token = take_while1 is_token
let is_digit = function '0' .. '9' -> true | _ -> false

(* From RFC 2045

        attribute := token
                     ; Matching of attributes
                     ; is ALWAYS case-insensitive.
*)
let attribute = token >>| String.lowercase_ascii

(* From RFC 2045

        ietf-token := <An extension token defined by a
                          standards-track RFC and registered
                          with IANA.>
        iana-token := <A publicly-defined extension token. Tokens
                          of this form must be registered with IANA
                          as specified in RFC 2048.>

   XXX(dinosaure): we don't check at this time if IETF/IANA token exists.
*)
let ietf_token = token

(* From RFC 2045

        x-token := <The two characters "X-" or "x-" followed, with
                       no intervening white space, by any token>
*)
let x_token =
  satisfy (function 'x' | 'X' -> true | _ -> false) *> char '-' *> token

(* From RFC 2045

        extension-token := ietf-token / x-token
*)
let extension_token =
  peek_char
  >>= function
  | Some 'X' | Some 'x' -> x_token >>| fun v -> `X_token v
  | _ -> ietf_token >>| fun v -> `Ietf_token v

(* From RFC 2045

        value := token / quoted-string
*)
let value =
  Rfc822.quoted_string
  >>| (fun v -> `String v)
  <|> (token >>| fun v -> `Token v)

let of_string s a =
  match parse_string a s with Ok v -> Some v | Error _ -> None

let disposition_type =
  token >>= fun s -> match String.lowercase_ascii s with
  | "inline" -> return `Inline
  | "attachment" -> return `Attachment
  | _ -> match of_string s extension_token with
    | Some v -> return v
    | None -> invalid_token s

(* From RFC 2045

        parameter := attribute "=" value
*)
let parameter =
  attribute
  >>= fun attribute -> option () Rfc822.cfws *> char '=' *> option () Rfc822.cfws *> value >>| fun value -> (attribute, value)

let quoted_date_time = value >>| fun _ -> ()

let parm parm value = string parm *> option () Rfc822.cfws *> char '=' *> option () Rfc822.cfws *> value
let filename_parm = parm "filename" value
let creation_date_parm = parm "creation-date" quoted_date_time
let modification_date_parm = parm "modification-date" quoted_date_time
let read_date_parm = parm "read-date" quoted_date_time
let size_parm = parm "read-date" (take_while1 is_digit >>| int_of_string)

let disposition_parm =
  (filename_parm >>| fun v -> Filename v)
  <|> (creation_date_parm >>| fun v -> Creation v)
  <|> (modification_date_parm >>| fun v -> Modification v)
  <|> (read_date_parm >>| fun v -> Read v)
  <|> (size_parm >>| fun v -> Size v)
  <|> (parameter >>| fun v -> Parameter v)

let disposition =
  option () Rfc822.cfws *> disposition_type <* option () Rfc822.cfws >>= fun ty ->
  many (option () Rfc822.cfws *> char ';' *> option () Rfc822.cfws *> disposition_parm) >>= fun parameters ->
  let filename = ref None in
  let creation = ref None in
  let modification = ref None in
  let read = ref None in
  let size = ref None in
  let parameters =
    List.fold_left
      (fun a -> function
         | Filename (`String v) | Filename (`Token v) -> filename := Some v ; a
         | Creation v -> creation := Some v ; a
         | Modification v -> modification := Some v ; a
         | Read v -> read := Some v ; a
         | Size v -> size := Some v ; a
         | Parameter v -> v :: a)
      [] parameters in
  return { ty; filename= !filename; creation= !creation; modification= !modification; read= !read; size= !size; parameters }
