open Multipart_form

let to_cohttp_header header =
  let header =
    Header.to_list header
    |> List.map @@ function
       | Field.Field (field_name, Content_type, v) ->
           ((field_name :> string), Content_type.to_string v)
       | Field (field_name, Content_encoding, v) ->
           ((field_name :> string), Content_encoding.to_string v)
       | Field (field_name, Content_disposition, v) ->
           ((field_name :> string), Content_disposition.to_string v)
       | Field (field_name, Field, v) ->
           ((field_name :> string), Unstrctrd.to_utf_8_string v) in
  Cohttp.Header.of_list header

module Client = struct
  let multipart_form v =
    let header, stream = Multipart_form.to_stream v in
    let response =
      let headers = to_cohttp_header header in
      Cohttp.Response.make ~headers () in
    let stream = Lwt_stream.from_direct stream in
    let body =
      Lwt_stream.map (fun (str, off, len) -> String.sub str off len) stream
    in
    (response, `Stream body)
end

let ( <.> ) f g x = f (g x)

module Server = struct
  let src = Logs.Src.create "multipart_form.cohttp"

  module Log = (val Logs.src_log src : Logs.LOG)

  let handler_exn exn =
    Log.err (fun m ->
        m "Got an exception when we tried to parsed a multipart/form: %S"
          (Printexc.to_string exn))

  let handler_thread th wk () =
    let open Lwt.Infix in
    th >>= function
    | Ok tree ->
        Lwt.wakeup_later wk tree ;
        Lwt.return_unit
    | Error (`Msg msg) ->
        Log.err (fun m -> m "Invalid multipart/form request: %s" msg) ;
        Lwt.return_unit

  let multipart_form ~identify req body =
    let content_type =
      let default =
        Content_type.make `Text (`Iana_token "plain")
          Content_type.Parameters.empty in
      Cohttp.Request.headers req |> fun hdrs ->
      Cohttp.Header.get hdrs "content-type"
      |> Option.map (fun content_type -> content_type ^ "\r\n")
      |> Option.map (Result.to_option <.> Content_type.of_string)
      |> Option.join
      |> Option.value ~default in
    let stream = Cohttp_lwt.Body.to_stream body in
    let identify = identify <.> to_cohttp_header in
    let `Parse th, stream =
      Multipart_form_lwt.stream ~identify stream content_type in
    let stream =
      Lwt_stream.map
        (fun (id, hdrs, stream) -> (id, to_cohttp_header hdrs, `Stream stream))
        stream in
    let rt, wk = Lwt.task () in
    Lwt.dont_wait (handler_thread th wk) handler_exn ;
    (`Tree rt, stream)
end
