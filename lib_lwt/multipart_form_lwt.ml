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

let blit src src_off dst dst_off len =
  Bigstringaf.blit_from_string src ~src_off dst ~dst_off ~len

let parse ~identify stream content_type =
  let ke = Ke.Rke.create ~capacity:0x1000 Bigarray.char in
  let output, push = Lwt_stream.create () in
  let emitters header =
    let stream, emitter = Lwt_stream.create () in
    let id = identify header in
    push (Some (id, header, stream)) ;
    (emitter, id) in
  ( `Parse
      (let rec go = function
         | Angstrom.Unbuffered.Done (_, tree) ->
             push None ;
             Lwt.return_ok tree
         | Fail _ ->
             push None ;
             Lwt.return_error (`Msg "Invalid multipart/form")
         | Partial { committed; continue } as state -> (
             Ke.Rke.N.shift_exn ke committed ;
             if committed = 0 then Ke.Rke.compress ke ;
             Lwt_stream.get stream >>= function
             | Some "" -> go state (* XXX(dinosaure): nothing to do. *)
             | Some str ->
                 Ke.Rke.N.push ke ~blit ~length:String.length ~off:0
                   ~len:(String.length str) str ;
                 let[@warning "-8"] (slice :: _) = Ke.Rke.N.peek ke in
                 go
                   (continue slice ~off:0 ~len:(Bigstringaf.length slice)
                      Incomplete)
             | None ->
             match Ke.Rke.N.peek ke with
             | [] -> go (continue Bigstringaf.empty ~off:0 ~len:0 Complete)
             | [ slice ] ->
                 go
                   (continue slice ~off:0 ~len:(Bigstringaf.length slice)
                      Complete)
             | slice :: _ ->
                 go
                   (continue slice ~off:0 ~len:(Bigstringaf.length slice)
                      Incomplete)) in
       go (Angstrom.Unbuffered.parse (parser ~emitters content_type))),
    output )
