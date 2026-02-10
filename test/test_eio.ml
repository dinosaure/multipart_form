let reporter ppf =
  let report src level ~over k msgf =
    let k _ =
      over () ;
      k () in
    let with_metadata header _tags k ppf fmt =
      Format.kfprintf k ppf
        ("[%a]%a[%a]: " ^^ fmt ^^ "\n%!")
        Fmt.(styled `Blue int)
        (Unix.getpid ()) Logs_fmt.pp_header (level, header)
        Fmt.(styled `Magenta string)
        (Logs.Src.name src) in
    msgf @@ fun ?header ?tags fmt -> with_metadata header tags k ppf fmt in
  { Logs.report }

let () = Fmt_tty.setup_std_outputs ~style_renderer:`Ansi_tty ~utf_8:true ()
let () = Logs.set_reporter (reporter Fmt.stderr)
let () = Logs.set_level ~all:true (Some Logs.Debug)

let truncated_request01 =
  {|--------------------------eb790219f130e103|}
  ^ "\r"
  ^ {|
Content-Disposition: form-data; name="text"|}
  ^ "\r"
  ^ {|
|}
  ^ "\r"
  ^ {|
default|}
  ^ "\r"
  ^ {|
--------------------------eb790219f130e103|}
  ^ "\r"
  ^ {|
Content-Disposition: form-data; name="file1"; filename="a.html"|}
  ^ "\r"
  ^ {|
Content-Type: text/html|}
  ^ "\r"
  ^ {|
|}
  ^ "\r"
  ^ {|
<!DOCTYPE html><title>Content of a.html.</title>|}
  ^ "\r"
  ^ {|
|}
  ^ "\r"
  ^ {|
--------------------------eb790219f130e103|}
  ^ "\r"
  ^ {|
Content-Disposition: form-data; name="file2"; filename="a.txt"|}
  ^ "\r"
  ^ {|
Content-Type: text/plain|}
  ^ "\r"
  ^ {|
|}
  ^ "\r"
  ^ {|
Conten|}

let truncated_request02 =
  {|--------------------------eb790219f130e103
Content-Disposition: form-data; name="text"

default
--------------------------eb790219f130e103
Content-Disposition: form-data; name="file1"; filename="a.html"
Content-Type: text/html

<!DOCTYPE html><title>Content of a.html.</title>

--------------------------eb790219f130e103
Content-Disposition: form-data; name="file2"; filename="a.txt"
Content-Type: text/plain

Conten|}

let always v _ = v

let test01 =
  Alcotest.test_case "truncated flow (with CRLF)" `Quick @@ fun _ ->
  Eio_main.run @@ fun _ ->
  Eio.Switch.run @@ fun sw ->
  let content_type =
    "multipart/form-data; boundary=------------------------eb790219f130e103\r\n"
  in
  let content_type =
    match Multipart_form.Content_type.of_string content_type with
    | Ok v -> v
    | Error (`Msg err) -> failwith err in
  let body = Eio.Flow.string_source truncated_request01 in
  let th =
    Multipart_form_eio.stream ~sw ~identify:(always ()) body content_type
    |> fst
    |> Eio.Promise.await_exn in
  Printf.printf "Got till promise!\n%!" ;
  match th with
  | Ok _ -> Alcotest.(check pass) "Truncated request" () ()
  | Error (`Msg err) -> Alcotest.failf "Unexpected error: %s" err

let test02 =
  Alcotest.test_case "truncated flow (without CRLF)" `Quick @@ fun _ ->
  Eio_main.run @@ fun _ ->
  Eio.Switch.run @@ fun sw ->
  let content_type =
    "multipart/form-data; boundary=------------------------eb790219f130e103\r\n"
  in
  let content_type =
    match Multipart_form.Content_type.of_string content_type with
    | Ok v -> v
    | Error (`Msg err) -> failwith err in
  let body = Eio.Flow.string_source truncated_request02 in
  let th =
    Multipart_form_eio.stream ~sw ~identify:(always ()) body content_type
    |> fst
    |> Eio.Promise.await_exn in
  match th with
  | Ok _ -> Alcotest.fail "Unexpected valid input"
  | Error (`Msg "Invalid multipart/form") ->
      Alcotest.(check pass) "truncated input" () ()
  | Error (`Msg err) -> Alcotest.failf "Unexpected error: %s." err

let () = Alcotest.run "multipart_form_eio" [ ("truncated", [ test01; test02 ]) ]
