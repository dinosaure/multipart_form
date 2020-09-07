let stream () =
  let buf = Buffer.create 0x100 in
  (buf, function Some str -> Buffer.add_string buf str | None -> ())

let gen =
  let v = ref (-1) in
  fun () ->
    incr v ;
    !v

let emitters () =
  let tbl = Hashtbl.create 0x100 in
  ( tbl,
    fun _header ->
      let idx = gen () in
      let buf, push = stream () in
      Hashtbl.add tbl idx buf ;
      (push, idx) )

let parser ~emitters =
  let open Angstrom in
  let open Multipart_form in
  Header.Decoder.header >>= fun header ->
  let content_type = Header.content_type header in
  parser ~emitters content_type

(* nc -l 8000
 * echo "Content of a.txt." > a.txt
 * echo "<!DOCTYPE html><title>Content of a.html.</title>" > a.html
 * curl -F "text=default" -F "file1=@a.html" -F "file2=@a.txt" localhost:8000 *)
let simple =
  {html|Host: localhost:8000
User-Agent: curl/7.47.0
Accept: */*
Content-Length: 489
Expect: 100-continue
Content-Type: multipart/form-data; boundary=------------------------eb790219f130e103

--------------------------eb790219f130e103
Content-Disposition: form-data; name="text"

default
--------------------------eb790219f130e103
Content-Disposition: form-data; name="file1"; filename="a.html"
Content-Type: text/html

<!DOCTYPE html><title>Content of a.html.</title>

--------------------------eb790219f130e103
Content-Disposition: form-data; name="file2"; filename="a.txt"
Content-Type: text/plain

Content of a.txt.

--------------------------eb790219f130e103--
|html}

let simple_without_header =
  {html|--------------------------eb790219f130e103
Content-Disposition: form-data; name="text"

default
--------------------------eb790219f130e103
Content-Disposition: form-data; name="file1"; filename="a.html"
Content-Type: text/html

<!DOCTYPE html><title>Content of a.html.</title>

--------------------------eb790219f130e103
Content-Disposition: form-data; name="file2"; filename="a.txt"
Content-Type: text/plain

Content of a.txt.

--------------------------eb790219f130e103--
|html}

module Map = Map.Make (String)

let to_map ~tbl m =
  let open Multipart_form in
  let rec go (map, rest) = function
    | Leaf { header; body } -> (
        match
          Option.bind
            (Header.content_disposition header)
            Content_disposition.name
        with
        | Some name ->
            (Map.add name (Buffer.contents (Hashtbl.find tbl body)) map, rest)
        | None -> (map, (body, Buffer.contents (Hashtbl.find tbl body)) :: rest)
        )
    | Multipart { body; _ } ->
        let fold acc = function Some elt -> go acc elt | None -> acc in
        List.fold_left fold (map, rest) body in
  go (Map.empty, []) m

let simple_multipart_form =
  Alcotest.test_case "simple" `Quick @@ fun () ->
  let tbl, emitters = emitters () in
  match Angstrom.parse_string ~consume:All (parser ~emitters) simple with
  | Ok m ->
      let m, r = to_map ~tbl m in
      let m = Map.bindings m in
      Alcotest.(check int) "unamed values" (List.length r) 0 ;
      Alcotest.(check string) "text" (List.assoc "text" m) "default" ;
      Alcotest.(check string)
        "file1" (List.assoc "file1" m)
        "<!DOCTYPE html><title>Content of a.html.</title>\n" ;
      Alcotest.(check string)
        "file2" (List.assoc "file2" m) "Content of a.txt.\n"
  | Error err -> Alcotest.fail err

let to_map ~assoc m =
  let open Multipart_form in
  let rec go (map, rest) = function
    | Leaf { header; body } -> (
        match
          Option.bind
            (Header.content_disposition header)
            Content_disposition.name
        with
        | Some name -> (Map.add name (List.assoc body assoc) map, rest)
        | None -> (map, (body, List.assoc body assoc) :: rest))
    | Multipart { body; _ } ->
        let fold acc = function Some elt -> go acc elt | None -> acc in
        List.fold_left fold (map, rest) body in
  go (Map.empty, []) m

let simple_with_helpers =
  Alcotest.test_case "simple with helpers" `Quick @@ fun () ->
  let open Multipart_form in
  let content_type =
    Rresult.R.get_ok
    @@ Content_type.of_string
         "multipart/form-data; \
          boundary=------------------------eb790219f130e103\r\n" in
  match of_string simple_without_header content_type with
  | Ok (m, assoc) ->
      let m, r = to_map ~assoc m in
      let m = Map.bindings m in
      Alcotest.(check int) "unamed values" (List.length r) 0 ;
      Alcotest.(check string) "text" (List.assoc "text" m) "default" ;
      Alcotest.(check string)
        "file1" (List.assoc "file1" m)
        "<!DOCTYPE html><title>Content of a.html.</title>\n" ;
      Alcotest.(check string)
        "file2" (List.assoc "file2" m) "Content of a.txt.\n"
  | Error (`Msg err) -> Alcotest.fail err

let () =
  Alcotest.run "multipart_form"
    [ ("multipart_form", [ simple_multipart_form; simple_with_helpers ]) ]
