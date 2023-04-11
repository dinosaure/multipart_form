module Client : sig
  val multipart_form :
    Multipart_form.multipart -> Cohttp.Header.t * Cohttp_lwt.Body.t
  (** [multipart_form v] returns a well-formed CoHTTP header with a CoHTTP body
      which can be used by the {i client} implementation of CoHTTP:

      {[
        let headers, body = Multipart_form_cohttp.Client.multipart_form v in
        let* resp, body = Client.post ~headers ~body uri in
        ...
      ]}

      For more details on constructing a {!type:Multipart_form.multipart}, take a
      look at {!module:Multipart_form}. *)
end

module Server : sig
  exception Invalid_multipart_form of string

  val multipart_form :
    identify:(Cohttp.Header.t -> 'id) ->
    Cohttp.Request.t ->
    Cohttp_lwt.Body.t ->
    [ `Tree of 'id Multipart_form.t Lwt.t ]
    * ('id * Cohttp.Header.t * Cohttp_lwt.Body.t) Lwt_stream.t
  (** [multipart_form ~identify req body] returns a {i fiber} which parses the
      given [body] and a stream of each part of the given [body]. The goal is
      to propose an easier interface specially for CoHTTP. By this way, this
      function expects a CoHTTP request and a CoHTTP body.

      To consume the returned stream, the returned {i fiber} must be launched:

      {[
        let consume_parts stream =
          let* part = Lwt_stream.get stream in
          match part with
          | None -> ...
          | Some (uid, headers, body) -> ...

        let handler _ req body =
          let `Tree th, stream = Multipart_form_cohttp.Server.multipart_form
            ~identify req body in
          Lwt.catch begin fun () ->
            let* tree, _ = Lwt.both (th, consume_parts stream) in
            ...
          end @@ function
          | Invalid_multipart_form err -> ...
      ]}

      In this example, we use [Lwt.both] to concurrently parse and consume
      parts. If the body is not a valid [multipart-form/data], the exception
      {!exn:Invalid_multipart_form} is raised. *)
end
