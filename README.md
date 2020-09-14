# Multipart-form

This library provides a /stream-way/ to extracts values from a
`multipart/form-data` HTTP 1.{0,1} document. The library does not requires an
HTTP server but can be used with one of them. The library is pretty
straightforward to co-exist with:
- [`cohttp`][cohttp]
- [`httpaf`][httpaf]

The `multipart/form-data` appears when you want to propose a form to the client.
You can encode values given by the client into the URL with
`application/x-www-form-urlencoded` or, if you want to let the client to upload
a file, `multipart/form-data` is required.

This library wants to process the second case.

## A /stream/ API

Due to the use of [`angstrom`][angstrom], `multipart_form` is able to manipulate
a /stream/ instead of process entirely the given document. Indeed, when the
client wants to upload a file, it will be a shame to store twice times the file
into the memory heap (firstly, keep entirely the `multipart/form-data` document,
secondly, extract the file from the first one).

So, the user is able to concurrently /eat/ the `multipart/form-data` document,
feed the `angstrom`'s state and save a part (such as the uploaded file) to
another location. By this way, we keep a control about memory consumption.

## Simple usage

`multipart_form` has its own definition of the `Content-Type` needed to start
the analyse of the given document. From `cohttp` or `httpaf`, you are able to
extract it as is and give it to `Multipart_form.Content_type.of_string` to get a
well-defined `Content-Type` value.

Then, you can start to /parse/ the document with this value and an `emitters`.

An `emitters` is a simple function which must give to the process a way to
/store/ the part and a unique identifier to be able to retrieve contents at the
end of the process:

```ocaml
type 'id emitters = unit -> 'id * (string option -> unit)
```

A use of `Lwt_stream.create` can gives you such required value.

## Status of the library

This library was done for my personal use and I probably missed some useful
elements for a basic user who wants to process a `multipart/form-data` document.
So, feel free to make an issue if you think that something is missed.

[cohttp]: https://github.com/mirage/ocaml-cohttp
[httpaf]: https://github.com/inhabitedtype/httpaf
[angstrom]: https://github.com/inhabitedtype/angstrom
