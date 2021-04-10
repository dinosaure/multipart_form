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
  let parser = parser ~emitters content_type in
  let state = ref (Angstrom.Buffered.parse parser) in
  let rec go () =
    if not (Queue.is_empty q) then begin
      (* Pop pending emits if any *)
      let (id, data) = Queue.pop q in
      let client_emitter = fst (Hashtbl.find tbl id) in
      client_emitter data >>= fun () ->
      go ()
    end else begin
      (* otherwise, continue parsing (thus adding elements to the queue) *)
      match !state with
      | Partial step ->
        Lwt_stream.get stream >>= fun data ->
        state := (step (
          match data with
          | None -> `Eof
          | Some str -> `String str
        ));
        go ()
      | Done (_, t) -> Lwt.return (Ok t)
      | Fail _ -> Lwt.return (Error (`Msg "Invalid input"))
    end
  in
  go () >>= fun res ->
  match res with
  | Ok t ->
    let client_id_of_id id = snd (Hashtbl.find tbl id) in
    Lwt.return (Ok (map client_id_of_id t))
  | Error e ->
    Lwt.return (Error e)
