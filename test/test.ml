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
        let filename =
          Option.bind
            (Header.content_disposition header)
            Content_disposition.filename in
        match
          Option.bind
            (Header.content_disposition header)
            Content_disposition.name
        with
        | Some name -> (Map.add name (filename, List.assoc body assoc) map, rest)
        | None -> (map, (body, (filename, List.assoc body assoc)) :: rest))
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
  match of_string_to_list simple_without_header content_type with
  | Ok (m, assoc) ->
      let m, r = to_map ~assoc m in
      let m = Map.bindings m in
      Alcotest.(check int) "unamed values" (List.length r) 0 ;
      Alcotest.(check string) "text" (snd (List.assoc "text" m)) "default" ;
      Alcotest.(check string)
        "file1"
        (snd (List.assoc "file1" m))
        "<!DOCTYPE html><title>Content of a.html.</title>\n" ;
      Alcotest.(check string)
        "file2"
        (snd (List.assoc "file2" m))
        "Content of a.txt.\n"
  | Error (`Msg err) -> Alcotest.fail err

let stream_of_string x =
  let once = ref false in
  let go () =
    if !once
    then None
    else (
      once := true ;
      Some (x, 0, String.length x)) in
  go

let random_string len =
  let res = Bytes.create len in
  for i = 0 to len - 1 do
    let code = Random.int (10 + 26 + 26) in
    if code < 10
    then Bytes.set res i (Char.chr (Char.code '0' + code))
    else if code < 10 + 16
    then Bytes.set res i (Char.chr (Char.code 'a' + code - 10))
    else Bytes.set res i (Char.chr (Char.code 'A' + code - (10 + 26)))
  done ;
  Bytes.unsafe_to_string res

let string_of_stream s =
  let buf = Buffer.create 0x100 in
  let rec go () =
    match s () with
    | None -> Buffer.contents buf
    | Some (str, off, len) ->
        Buffer.add_substring buf str off len ;
        go () in
  go ()

let make_simple_multipart =
  Alcotest.test_case "simple" `Quick @@ fun () ->
  let open Multipart_form in
  let part0 =
    part
      ~disposition:(Content_disposition.v ~filename:"a.html" "file1")
      ~encoding:`Base64
      (stream_of_string "<!DOCTYPE html><title>Content of a.html.</title>\n")
  in
  let part1 =
    part
      ~disposition:(Content_disposition.v ~filename:"a.txt" "file2")
      (stream_of_string "Content of a.txt.\n") in
  let t = multipart ~rng:(fun ?g:_ len -> random_string len) [ part0; part1 ] in
  let header, stream = to_stream t in
  let str = string_of_stream stream in
  match of_string_to_list str (Header.content_type header) with
  | Ok (m, assoc) ->
      let m, _ = to_map ~assoc m in
      let m = Map.bindings m in
      Alcotest.(check string)
        "file1"
        (snd (List.assoc "file1" m))
        "<!DOCTYPE html><title>Content of a.html.</title>\n" ;
      Alcotest.(check string)
        "file2"
        (snd (List.assoc "file2" m))
        "Content of a.txt.\n"
  | Error (`Msg err) -> Alcotest.fail err

let parse_content_type x = Multipart_form.Content_type.of_string (x ^ "\r\n")

let content_type =
  Alcotest.testable Multipart_form.Content_type.pp
    Multipart_form.Content_type.equal

let make raw expect =
  Alcotest.test_case raw `Quick @@ fun () ->
  match parse_content_type raw with
  | Ok value -> Alcotest.(check content_type) raw expect value
  | Error err ->
      Fmt.invalid_arg "Invalid content-type value: %s (%a)." raw
        Rresult.R.pp_msg err

let content_type_0 =
  let open Multipart_form.Content_type in
  let value =
    let open Rresult.R in
    Parameters.key "charset" >>= fun charset ->
    Parameters.value "us-ascii" >>= fun us_ascii ->
    Subtype.iana "plain" >>| fun subty ->
    make `Text subty Parameters.(add charset us_ascii empty) in
  Rresult.R.get_ok value

let content_type_1 =
  let open Multipart_form.Content_type in
  let value =
    let open Rresult.R in
    Parameters.key "charset" >>= fun charset ->
    Parameters.value "us-ascii" >>= fun us_ascii ->
    Subtype.iana "plain" >>| fun subty ->
    make `Text subty Parameters.(add charset us_ascii empty) in
  Rresult.R.get_ok value

let content_type_2 =
  let open Multipart_form.Content_type in
  let value =
    let open Rresult.R in
    Parameters.key "charset" >>= fun charset ->
    Parameters.value (Rosetta.encoding_to_string `ISO_8859_1) >>= fun latin1 ->
    Subtype.iana "plain" >>| fun subty ->
    make `Text subty Parameters.(add charset latin1 empty) in
  Rresult.R.get_ok value

let contents =
  {multipart|
-----------------------------11410681503802810592492044004
Content-Disposition: form-data; name="dream.csrf"

foobar
-----------------------------11410681503802810592492044004
Content-Disposition: form-data; name="files"; filename="a b.txt"
Content-Type: text/plain

1234
-----------------------------11410681503802810592492044004
Content-Disposition: form-data; name="files"; filename="a b.txt"
Content-Type: text/plain

4321
-----------------------------11410681503802810592492044004--
|multipart}

let filename_with_space =
  Alcotest.test_case "filename with space" `Quick @@ fun () ->
  let content_type =
    let open Multipart_form.Content_type in
    make `Multipart (`Iana_token "form-data")
      Parameters.(
        add (key_exn "boundary")
          (value_exn "---------------------------11410681503802810592492044004")
          Map.empty) in
  match Multipart_form.of_string_to_list contents content_type with
  | Ok (m, assoc) ->
      let map, _ = to_map ~assoc m in
      Alcotest.(check bool)
        "files"
        (Map.exists (fun str _ -> str = "files") map)
        true ;
      let filename, _body = Map.find "files" map in
      Alcotest.(check (option string)) "filename" (Some "a b.txt") filename
  | Error (`Msg err) -> Alcotest.failf "parser: %s." err

let () =
  Alcotest.run "multipart_form"
    [
      ( "content-type",
        [
          make "text/plain; charset=us-ascii (Plain text)" content_type_0;
          make "text/plain; charset=\"us-ascii\"" content_type_1;
          make "text/plain; charset=ISO-8859-1" content_type_2;
        ] );
      ( "multipart_form (decoder)",
        [ simple_multipart_form; simple_with_helpers ] );
      ("multipart_form (encoder)", [ make_simple_multipart ]);
      ("filename with space", [ filename_with_space ]);
    ]
