open Multipart_form
open Lwt.Infix

let parse ~on_part stream content_type =
  let q = Queue.create () in
  let fresh_id =
    let r = ref 0 in
    fun () -> incr r; !r
  in
  let tbl = Hashtbl.create 0x10 in
  let emitters header =
    let id = fresh_id () in
    let client_emitter, client_id = on_part header in
    Hashtbl.add tbl id (client_emitter, client_id);
    (fun data -> Queue.push (id, data) q), id
  in
  let parse = Multipart_form.parse ~emitters content_type in
  let rec go () =
    if not (Queue.is_empty q) then begin
      (* Pop pending emits if any *)
      let (id, data) = Queue.pop q in
      let client_emitter = fst (Hashtbl.find tbl id) in
      client_emitter data >>= fun () ->
      go ()
    end else begin
      (* otherwise, continue parsing (thus adding elements to the queue) *)
      Lwt_stream.get stream >>= fun data ->
      let data = match data with Some s -> `String s | None -> `Eof in
      match parse data with
      | `Continue -> go ()
      | `Done t ->
        let client_id_of_id id = snd (Hashtbl.find tbl id) in
        Lwt.return_ok (map client_id_of_id t)
      | `Fail _ ->
        Lwt.return_error (`Msg "Invalid multipart/form")
    end
  in
  go ()

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
           Lwt.return_error (`Msg "Invalid multipart/form")
       in go ()),
    output )
