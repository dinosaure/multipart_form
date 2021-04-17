open Multipart_form
open Lwt.Infix

let stream_with_bounds ?(bounds = 10) ~identify stream content_type =
  let output, push = Lwt_stream.create () in
  let q = Queue.create () in
  let fresh_id =
    let r = ref 0 in
    fun () ->
      incr r ;
      !r in
  let tbl = Hashtbl.create 0x10 in
  let emitters header =
    let id = fresh_id () in
    Queue.push (`Id (header, id)) q ;
    ((fun data -> Queue.push (`Data (id, data)) q), id) in
  let parse = Multipart_form.parse ~emitters content_type in
  let rec go () =
    match Queue.pop q with
    | `Id (header, id) ->
        let client_id = identify header in
        let stream, bounded_emitter = Lwt_stream.create_bounded bounds in
        Hashtbl.add tbl id (client_id, stream, bounded_emitter) ;
        push (Some (client_id, header, stream)) ;
        go ()
    | `Data (id, Some data) ->
        let _, _, emitter = Hashtbl.find tbl id in
        emitter#push data >>= fun () -> go ()
    | `Data (id, None) ->
        let _, _, emitter = Hashtbl.find tbl id in
        emitter#close ;
        go ()
    | exception Queue.Empty -> (
        (* otherwise, continue parsing (thus adding elements to the queue) *)
        Lwt_stream.get stream
        >>= fun data ->
        let data = match data with Some s -> `String s | None -> `Eof in
        match parse data with
        | `Continue -> go ()
        | `Done t ->
            let client_id_of_id id =
              let client_id, _, _ = Hashtbl.find tbl id in
              client_id in
            push None ;
            Lwt.return_ok (map client_id_of_id t)
        | `Fail _ ->
            push None ;
            Lwt.return_error (`Msg "Invalid multipart/form")) in
  (`Parse (go ()), output)

let stream ~identify stream content_type =
  let output, push = Lwt_stream.create () in
  let emitters header =
    let stream, emitter = Lwt_stream.create () in
    let id = identify header in
    push (Some (id, header, stream)) ;
    (emitter, id) in
  let parse = Multipart_form.parse ~emitters content_type in
  ( `Parse
      (let rec go () =
         Lwt_stream.get stream >>= fun data ->
         let data = match data with Some s -> `String s | None -> `Eof in
         match parse data with
         | `Continue -> go ()
         | `Done tree ->
             push None ;
             Lwt.return_ok tree
         | `Fail _ ->
             push None ;
             Lwt.return_error (`Msg "Invalid multipart/form") in
       go ()),
    output )
