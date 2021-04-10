open Multipart_form

val parse :
  on_part:(Header.t -> (string option -> unit Lwt.t) * 'id) ->
  string Lwt_stream.t ->
  Content_type.t ->
  ('id t, [> `Msg of string ]) Lwt_result.t
