open Multipart_form

let stream ~sw ?(bounds = 10) ?(buffer_size = 8192) ~identify flow content_type
    =
  let output = Eio.Stream.create max_int in
  let q = Queue.create () in
  let buf = Cstruct.create buffer_size in
  let fresh_id =
    let r = ref 0 in
    fun () ->
      incr r ;
      !r in
  let emitters header =
    let id = fresh_id () in
    Queue.push (`Id (header, id)) q ;
    let push = function
      | None -> Queue.push (`End_of_part id) q
      | Some data -> Queue.push (`Data (id, data)) q in
    (push, id) in
  let parse = Multipart_form.parse ~emitters content_type in
  let tbl = Hashtbl.create 0x10 in
  let rec go () =
    match Queue.pop q with
    | `Id (header, id) ->
        let client_id = identify header in
        let stream = Eio.Stream.create bounds in
        Hashtbl.add tbl id (client_id, stream) ;
        Eio.Stream.add output (Some (client_id, header, stream)) ;
        go ()
    | `Data (id, data) ->
        let _, stream = Hashtbl.find tbl id in
        Eio.Stream.add stream (Some data) ;
        go ()
    | `End_of_part id ->
        let _, stream = Hashtbl.find tbl id in
        Eio.Stream.add stream None ;
        go ()
    | exception Queue.Empty -> (
        let data =
          try
            let n = Eio.Flow.single_read flow buf in
            `String (Cstruct.to_string (Cstruct.sub buf 0 n))
          with End_of_file -> `Eof in
        match parse data with
        | `Continue -> go ()
        | `Done t ->
            let client_id_of_id id =
              let client_id, _ = Hashtbl.find tbl id in
              client_id in
            Eio.Stream.add output None ;
            Result.Ok (map client_id_of_id t)
        | `Fail _ -> Result.Error (`Msg "Invalid multipart/form")) in
  (Eio.Fiber.fork_promise ~sw go, output)

(* only used internally to implement of_stream_to_{tree,list} *)
let of_flow_to_tbl f content_type =
  let identify =
    let id = ref (-1) in
    fun _header ->
      incr id ;
      !id in
  Eio.Switch.run @@ fun sw ->
  let t, parts = stream ~sw ~identify f content_type in
  let parts_tbl = Hashtbl.create 0x10 in
  let rec consume_part () =
    match Eio.Stream.take parts with
    | None -> ()
    | Some (id, _, part_stream) ->
        let rec drain acc =
          match Eio.Stream.take part_stream with
          | None -> String.concat "" (List.rev acc)
          | Some chunk -> drain (chunk :: acc) in
        Hashtbl.add parts_tbl id (drain []) ;
        consume_part () in
  Eio.Fiber.fork ~sw consume_part ;
  Eio.Promise.await_exn t |> Result.map (fun tree -> (tree, parts_tbl))

let of_flow_to_tree f content_type =
  of_flow_to_tbl f content_type
  |> Result.map (fun (tree, parts_tbl) -> map (Hashtbl.find parts_tbl) tree)

let of_flow_to_list f content_type =
  of_flow_to_tbl f content_type
  |> Result.map (fun (tree, parts_tbl) ->
      let assoc = Hashtbl.fold (fun k b a -> (k, b) :: a) parts_tbl [] in
      (tree, assoc))
