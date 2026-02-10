open Multipart_form

(** {3 Streaming API.} *)

val stream :
  sw:Eio.Switch.t ->
  ?bounds:int ->
  ?buffer_size:int ->
  identify:(Header.t -> 'id) ->
  _ Eio.Flow.source ->
  Content_type.t ->
  ('id t, [> `Msg of string ]) result Eio.Promise.or_exn
  * ('id * Header.t * string option Eio.Stream.t) option Eio.Stream.t
(** [stream ~identify flow content_type] returns:
    - a promise [th] about the parser
    - a stream of parts (None signals end of parts)

    Each part contains an id, header, and content stream where None signals 
    end of part content.

    Example saving parts to files:
    {[
      val save_part : filename:string -> Header.t -> string option Eio.Stream.t -> unit

      Eio.Switch.run @@ fun sw ->
      let identify _ : string = random_unique_filename () in
      let th, parts = stream ~sw ~identify flow content_type in
      let rec save_parts () =
        match Eio.Stream.take parts with
        | None -> ()  (* No more parts *)
        | Some (filename, hdr, contents) ->
            save_part ~filename hdr contents;
            save_parts ()
      in
      Eio.Fiber.fork ~sw save_parts;
      Eio.Promise.await_exn th
    ]}

    Parts are produced as the input is parsed. The promise resolves when 
    parsing completes or fails. *)

(** {3 Non-streaming API.}

    These functions will store the entire multipart contents in memory,
    and therefore should not be used when handling possibly large data. *)

val of_flow_to_list :
  _ Eio.Flow.source ->
  Content_type.t ->
  (int t * (int * string) list, [> `Msg of string ]) result
(** Similar to [Multipart_form.of_string_to_list], but consumes an
    [Eio.Flow.source]. *)

val of_flow_to_tree :
  _ Eio.Flow.source -> Content_type.t -> (string t, [> `Msg of string ]) result
(** [of_flow_to_tree flow content_type] returns, if it succeeds, a value
    {!Multipart_form.t} representing the multipart document, where the contents 
    of the parts are stored as strings. It is equivalent to [of_flow_to_list] 
    where references have been replaced with their associated contents. *)
