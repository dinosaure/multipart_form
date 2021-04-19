# `Multipart_form`

`multipart_form` is a little library which allows to parse and generate
the body of an HTTP request with several parts. The library wants to be
agnostic to the underlying HTTP implementation used. As far as the HTTP
implementation is able to give you a _stream_ of the body and the
`Content-Type` value, you are able use `multipart_form`.

`multipart_form` is specially designed to _stream_ the process to parse
or emit a `multipart/form-data` body.

For example, if you want to upload a file, with others implementations,
you are mainly limited by the memory consumption. If you want to upload
a file of 4 Go for example, you need 4 Go in memory...

`multipart_form` wants to pass to a /stream/ as soon as it can parts. The
best example is available with `multipart_form.lwt`:
```ocaml
val stream :
  ?bounds:int ->
  ?identify:(Multipart_form.Header.t -> 'id) ->
  string Lwt_stream.t ->
  Multipart_form.Content_type.t ->
  [ `Parse of ('id Multipart_form.t, [> `Msg of string ]) result Lwt.t ]
  * ('id * Multipart_form.Header.t * string Lwt_stream.t) Lwt_stream.t
```

With such function, the user is able to pass a _stream_ of the body
(which can be directly connected to the `read` _syscall_). Then, the
function return a _stream_ of parts and a promise of the end of the
parsing. By this way, the user can concurrently execute both:
```ocaml
val save_into_a_file : filename:string -> string Lwt_stream.t -> unit Lwt.t
val random_unique_filename : unit -> string

let request_handler content_type body =
  let identify _ = random_unique_filename () in
  let `Parse th, stream = stream ~identify body content_type in
  let rec saves () = Lwt_stream.get stream >>= function
    | None -> Lwt.return_unit
    | Some (filename, _hdr, stream) ->
      save_into_a_file ~filename stream >>= saves in
  Lwt.both th (saves ()) >>= fun (res, ()) ->
  Lwt.return res
```

Such code saves parts into files and it runs concurrently with the
parsing `th`. /streams/ are bounded (see `Lwt_stream.create_bounded`).
So, if one is full, we enforce `lwt` to execute `save_into_a_file` to
consume it. By this way, we keep a stable usage of memories.

## Agnostic to the scheduler

Even if `multipart_form.lwt` is easy to use, the distribution comes with
a lower level of abstraction without `lwt`. By this way, the user is able
to _extend_ `multipart_form` with `async` or something else if it want.

The lower-level of the API provides multiples functions to help to user
to manipulate the result:
```ocaml
type 'id t =
  | Leaf of 'id elt
  | Multipart of 'id t option list elt
and 'id elt = { header : Header.t; body : 'id }

val map : ('a -> 'b) -> 'a t -> 'b t
val flatten : 'a t -> 'a elt list
```

## An encoder

The library is able to generate a `multipart/form-data` if you wants.
You can create several parts which contain a _stream_ of their bodies. Then,
you are able to compose these parts into one a single `multipart` object.
Finally, `multipart_form` is able to produce a stream:
```ocaml
val stream_of_string : string -> (string * int * int) stream
val stream_of_file : string -> (string * int * int) stream
val rng : int -> string

let part0 = part
  ~encoding:`Base64
  ~disposition:(Content_disposition.v ~filename:"image.png" ~kind:`Attachment "my-image"
  (stream_of_file "image.png")

let part1 = part
  ~disposition:(Content_disposition.v ~kind:`Inline "my-text"
  (stream_of_string "My super image!")

let contents = multipart ~rng [ part0; part1 ]

let rec to_output stream oc = match stream () with
  | Some (str, off, len) ->
    output_substring oc str off len ; to_output stream oc
  | None -> ()

let () = to_output (to_stream contents) stdout
```
