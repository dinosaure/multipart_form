open Multipart_form

val stream :
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
    a mapping between temporary files and parts.

    {b NOTE:} This function does not have any bounds about memory consumption.
    By this way, for huge file, even if we ensure to {i interlace} the given
    stream and the returned stream with [Lwt.both], we don't create an
    opportunity to collect garbages and the limit will be only your memory.

    If you really care about memory consumption, we advise you to use
    {!stream_with_bounds} which limits memory consumptions of streams. *)

val stream_with_bounds :
  ?bounds:int ->
  identify:(Header.t -> 'id) ->
  string Lwt_stream.t ->
  Content_type.t ->
  [ `Parse of ('id t, [> `Msg of string ]) result Lwt.t ]
  * ('id * Header.t * string Lwt_stream.t) Lwt_stream.t
(** [stream_with_bounds ?bounds ~identify src content_type] is exactly
    {!stream} with bounds on memory consumption. That's say we ensure
    to give an opportunity to [lwt] to re-schedule promises and to give
    an opportunity to collect garbage with a desired limit [bounds]. *)
