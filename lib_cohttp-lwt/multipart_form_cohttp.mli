module Client : sig
  val multipart_form :
    Multipart_form.multipart -> Cohttp.Response.t * Cohttp_lwt.Body.t
end

module Server : sig
  val multipart_form :
    identify:(Cohttp.Header.t -> 'id) ->
    Cohttp.Request.t ->
    Cohttp_lwt.Body.t ->
    [ `Tree of 'id Multipart_form.t Lwt.t ]
    * ('id * Cohttp.Header.t * Cohttp_lwt.Body.t) Lwt_stream.t
end
