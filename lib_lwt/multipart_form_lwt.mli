open Multipart_form

(** {3 Streaming API.} *)

val stream :
  ?bounds:int ->
  identify:(Header.t -> 'id) ->
  string Lwt_stream.t ->
  Content_type.t ->
  [ `Parse of ('id t, [> `Msg of string ]) result Lwt.t ]
  * ('id * Header.t * string Lwt_stream.t) Lwt_stream.t
(** [stream ~identify src content_type] returns:
    - a promise [th] about the parser
    - a stream [stream] of parts

    They can be manipulated with [Lwt.both], and, by this way,
    ensure a real stream between the parser [th] and the process
    which saves parts.

    Assume that you have a function to save a [Lwt_stream.t] into
    a [filename]:
    {[
      val save_part : filename:string -> Header.t -> string Lwt_stream.t ->
        unit Lwt.t
    ]}

    You can use it with [stream] like:
    {[
      let identify _ : string = random_unique_filename () in
      let `Parse th, stream = stream ~identify src content_type in
      let rec saves () = Lwt_stream.get stream >>= function
        | None -> Lwt.return_unit
        | Some (filename, hdr, contents) ->
          save_part ~filename hdr contents >>= fun () ->
          saves () in
      Lwt.both th (saves ()) >>= fun (res, ()) -> Lwt.return res
    ]}

    By this way, as long as we parse [src], at the same time, we save parts
    into filenames. Finally, we return the [multipart/form] structure with
    a mapping between temporary files and parts. *)

(** {3 Non-streaming API.}

    These functions will store the entire multipart contents in memory,
    and therefore should not be used when handling possibly large data. *)

val of_stream_to_list :
  string Lwt_stream.t ->
  Content_type.t ->
  (int t * (int * string) list, [> `Msg of string ]) result Lwt.t
(** Similar to [Multipart_form.of_stream_to_list], but consumes a
   [Lwt_stream.t]. *)

val of_stream_to_tree :
  string Lwt_stream.t ->
  Content_type.t ->
  (string t, [> `Msg of string ]) result Lwt.t
(** [of_stream_to_tree stream content_type] returns, if it succeeds, a value
   {!Multipart_form.t} representing the multipart document, where the contents of the parts are
   stored as strings. It is equivalent to [of_stream_to_list] where references
   have been replaced with their associated contents. *)
