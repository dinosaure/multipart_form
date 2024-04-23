open Multipart_form

module Bounded_stream = struct
  type 'a t = {
    buffer : 'a array;
    mutable rd_pos : int;
    mutable wr_pos : int;
    lock : Miou.Mutex.t;
    non_empty : Miou.Condition.t;
    non_full : Miou.Condition.t;
    mutable closed : bool;
  }

  let create size v =
    let lock = Miou.Mutex.create () in
    let non_empty = Miou.Condition.create () in
    let non_full = Miou.Condition.create () in
    {
      buffer = Array.make size v;
      lock;
      rd_pos = 0;
      wr_pos = 0;
      non_empty;
      non_full;
      closed = false;
    }

  let put t data =
    Miou.Mutex.protect t.lock @@ fun () ->
    match (data, t.closed) with
    | None, _ ->
        t.closed <- true ;
        Miou.Condition.signal t.non_empty
    | Some _, true -> invalid_arg "Bounded_stream.put: closed stream"
    | Some data, _ ->
        while (t.wr_pos + 1) mod Array.length t.buffer = t.rd_pos do
          Miou.Condition.wait t.non_full t.lock
        done ;
        t.buffer.(t.wr_pos) <- data ;
        t.wr_pos <- (t.wr_pos + 1) mod Array.length t.buffer ;
        Miou.Condition.signal t.non_empty

  let get t =
    Miou.Mutex.protect t.lock @@ fun () ->
    while t.wr_pos = t.rd_pos && not t.closed do
      Miou.Condition.wait t.non_empty t.lock
    done ;
    if t.closed && t.wr_pos = t.rd_pos
    then None
    else
      let data = t.buffer.(t.rd_pos) in
      t.rd_pos <- (t.rd_pos + 1) mod Array.length t.buffer ;
      Miou.Condition.signal t.non_full ;
      Some data

  let rec iter fn t =
    match get t with
    | None -> ()
    | Some v ->
        let prm = Miou.call_cc @@ fun () -> fn v in
        Miou.await_exn prm ;
        iter fn t
end

let closed_stream =
  let stream = Bounded_stream.create 0 String.empty in
  Bounded_stream.put stream None ;
  stream

let stream ?(bounds = 10) ~identify ~epsilon stream content_type =
  let epsilon = (epsilon, Header.of_list [], closed_stream) in
  let output = Bounded_stream.create bounds epsilon in
  let q = Queue.create () in
  let fresh_id =
    let r = Atomic.make 0 in
    fun () -> Atomic.fetch_and_add r 1 in
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
        let stream = Bounded_stream.create bounds String.empty in
        Hashtbl.add tbl id (client_id, stream) ;
        Bounded_stream.put output (Some (client_id, header, stream)) ;
        go ()
    | `Data (id, data) ->
        let _, emitter = Hashtbl.find tbl id in
        Bounded_stream.put emitter data ;
        go ()
    | exception Queue.Empty -> (
        let data = Bounded_stream.get stream in
        let data = match data with Some s -> `String s | None -> `Eof in
        match parse data with
        | `Continue -> go ()
        | `Done t ->
            let client_id_of_id id =
              let client_id, _ = Hashtbl.find tbl id in
              client_id in
            Bounded_stream.put output None ;
            Ok (map client_id_of_id t)
        | `Fail _ ->
            Bounded_stream.put output None ;
            Error (`Msg "Invalid multipart/form")) in
  let prm = Miou.call_cc go in
  (`Parse prm, output)

let of_stream_to_tbl v content_type =
  let identify =
    let id = Atomic.make 0 in
    fun _header -> Atomic.fetch_and_add id 1 in
  let `Parse prm, parts = stream ~identify ~epsilon:(-1) v content_type in
  let parts_tbl = Hashtbl.create 0x10 in
  let consume_part (id, _, part_stream) =
    let buf = Buffer.create 0x1000 in
    Bounded_stream.iter (Buffer.add_string buf) part_stream ;
    Hashtbl.add parts_tbl id (Buffer.contents buf) in
  Bounded_stream.iter consume_part parts ;
  let res = Miou.await_exn prm in
  Result.map (fun tree -> (tree, parts_tbl)) res

let of_stream_to_list stream content_type =
  let res = of_stream_to_tbl stream content_type in
  Result.map
    (fun (tree, parts_tbl) ->
      let assoc = Hashtbl.fold (fun k b a -> (k, b) :: a) parts_tbl [] in
      (tree, assoc))
    res
