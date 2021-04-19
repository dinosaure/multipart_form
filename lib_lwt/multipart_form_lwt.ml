open Multipart_form
open Lwt.Infix

let stream ?(bounds = 10) ~identify stream content_type =
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

(* only used internally to implement of_stream_to_{tree,list} *)
let of_stream_to_tbl s content_type =
  let identify =
    let id = ref (-1) in
    fun _header ->
      incr id ;
      !id in
  let `Parse t, parts = stream ~identify s content_type in
  let parts_tbl = Hashtbl.create 0x10 in
  let consume_part (id, _, part_stream) =
    let buf = Buffer.create 4096 in
    Lwt_stream.iter (Buffer.add_string buf) part_stream >>= fun () ->
    Hashtbl.add parts_tbl id (Buffer.contents buf) ;
    Lwt.return () in
  Lwt.both t (Lwt_stream.iter_s consume_part parts) >>= fun (res, ()) ->
  Lwt.return @@ Result.map (fun tree -> (tree, parts_tbl)) res

let of_stream_to_tree s content_type =
  of_stream_to_tbl s content_type >>= fun res ->
  Lwt.return
  @@ Result.map (fun (tree, parts_tbl) -> map (Hashtbl.find parts_tbl) tree) res

let of_stream_to_list s content_type =
  of_stream_to_tbl s content_type >>= fun res ->
  Lwt.return
  @@ Result.map
       (fun (tree, parts_tbl) ->
         let assoc = Hashtbl.fold (fun k b a -> (k, b) :: a) parts_tbl [] in
         (tree, assoc))
       res
