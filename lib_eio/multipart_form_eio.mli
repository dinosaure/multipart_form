open Multipart_form

(** {3 Streaming API.} *)

val stream :
  sw:Eio.Switch.t ->
  ?bounds:int ->
  identify:(Header.t -> 'id) ->
  string Eio.Stream.t ->
  Content_type.t ->
  ('id t, [> `Msg of string ]) result Eio.Promise.or_exn
  * ('id * Header.t * string Eio.Stream.t) Eio.Stream.t
(** [stream ~identify src content_type] returns:
    - a promise [th] about the parser
    - a stream [stream] of parts

    To ensure all elements of the stream are consumed, use [Fiber.fork] with
    same switch that repeatedly [Eio.Stream.take]s from the stream.
    This ensures that all items are read from the stream.

    Assume that you have a function to save a [Eio.stream.t] into
    a [filename]:
    {[
      val save_part : filename:string -> Header.t -> string Eio.Stream.t ->
        unit
    ]}

    You can use it with [stream] like:
    {[
      Eio.Switch.run @@ fun sw ->
      let identify _ : string = random_unique_filename () in
      let th, stream = stream ~sw ~identify src content_type in
      let rec saves () =
        let (filename, hdr, contents) = match Eio.Stream.take stream in
        save_part ~filename hdr contents;
        saves () in
      Eio.Fiber.fork ~sw saves;
      Eio.Promise.await_exn th
    ]}

    By this way, as long as we parse [src], at the same time, we save parts
    into filenames. Finally, we return the [multipart/form] structure with
    a mapping between temporary files and parts. *)

(** {3 Non-streaming API.}

    These functions will store the entire multipart contents in memory,
    and therefore should not be used when handling possibly large data. *)

val of_stream_to_list :
  string Eio.Stream.t ->
  Content_type.t ->
  (int t * (int * string) list, [> `Msg of string ]) result
(** Similar to [Multipart_form.of_stream_to_list], but consumes a
   [Eio.Stream.t]. *)

val of_stream_to_tree :
  string Eio.Stream.t ->
  Content_type.t ->
  (string t, [> `Msg of string ]) result
(** [of_stream_to_tree stream content_type] returns, if it succeeds, a value
   {!Multipart_form.t} representing the multipart document, where the contents of the parts are
   stored as strings. It is equivalent to [of_stream_to_list] where references
   have been replaced with their associated contents. *)
