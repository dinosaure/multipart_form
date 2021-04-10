open Stdlib
module Field_name = Field_name
module Field = Field
module Header = Header
module Content_type = Content_type
module Content_encoding = Content_encoding
module Content_disposition = Content_disposition

type 'a stream = unit -> 'a option

module B64 = struct
  open Angstrom

  let parser ~write_data end_of_body =
    let dec = Base64_rfc2045.decoder `Manual in

    let check_end_of_body =
      let expected_len = String.length end_of_body in
      Unsafe.peek expected_len (fun ba ~off ~len ->
          let raw = Bigstringaf.substring ba ~off ~len in
          String.equal raw end_of_body) in

    let trailer () =
      let rec finish () =
        match Base64_rfc2045.decode dec with
        | `Await -> assert false
        | `Flush data ->
            write_data data ;
            finish ()
        | `Malformed err -> fail err
        | `Wrong_padding -> fail "wrong padding"
        | `End -> commit
      and go () =
        match Base64_rfc2045.decode dec with
        | `Await ->
            Base64_rfc2045.src dec Bytes.empty 0 0 ;
            finish ()
        | `Flush data ->
            write_data data ;
            go ()
        | `Malformed err -> fail err
        | `Wrong_padding -> fail "wrong padding"
        | `End -> commit in

      go () in

    fix @@ fun m ->
    let choose chunk = function
      | true ->
          let chunk = Bytes.sub chunk 0 (Bytes.length chunk - 1) in
          Base64_rfc2045.src dec chunk 0 (Bytes.length chunk) ;
          trailer ()
      | false ->
          Bytes.set chunk (Bytes.length chunk - 1) end_of_body.[0] ;
          Base64_rfc2045.src dec chunk 0 (Bytes.length chunk) ;
          advance 1 *> m in

    Unsafe.take_while (( <> ) end_of_body.[0]) Bigstringaf.substring
    >>= fun chunk ->
    let rec go () =
      match Base64_rfc2045.decode dec with
      | `End -> commit
      | `Await ->
          let chunk' = Bytes.create (String.length chunk + 1) in
          Bytes.blit_string chunk 0 chunk' 0 (String.length chunk) ;
          check_end_of_body >>= choose chunk'
      | `Flush data ->
          write_data data ;
          go ()
      | `Malformed err -> fail err
      | `Wrong_padding -> fail "wrong padding" in
    go ()

  let with_emitter ~emitter end_of_body =
    let write_data x = emitter (Some x) in
    parser ~write_data end_of_body

  let to_end_of_input ~write_data =
    let dec = Base64_rfc2045.decoder `Manual in

    fix @@ fun m ->
    match Base64_rfc2045.decode dec with
    | `End -> commit
    | `Await -> (
        peek_char >>= function
        | None ->
            Base64_rfc2045.src dec Bytes.empty 0 0 ;
            return ()
        | Some _ ->
            available >>= fun n ->
            Unsafe.take n (fun ba ~off ~len ->
                let chunk = Bytes.create len in
                Bigstringaf.blit_to_bytes ba ~src_off:off chunk ~dst_off:0 ~len ;
                Base64_rfc2045.src dec chunk 0 len)
            >>= fun () -> m)
    | `Flush data ->
        write_data data ;
        m
    | `Malformed err -> fail err
    | `Wrong_padding -> fail "wrong padding"
end

module RAW = struct
  open Angstrom

  let parser ~write_line end_of_body =
    let check_end_of_body =
      let expected_len = String.length end_of_body in
      Unsafe.peek expected_len (fun ba ~off ~len ->
          let raw = Bigstringaf.substring ba ~off ~len in
          String.equal raw end_of_body) in

    fix @@ fun m ->
    let choose chunk = function
      | true ->
          let chunk = Bytes.sub_string chunk 0 (Bytes.length chunk - 1) in
          write_line chunk ;
          commit
      | false ->
          Bytes.set chunk (Bytes.length chunk - 1) end_of_body.[0] ;
          write_line (Bytes.unsafe_to_string chunk) ;
          advance 1 *> m in

    Unsafe.take_while (( <> ) end_of_body.[0]) Bigstringaf.substring
    >>= fun chunk ->
    let chunk' = Bytes.create (String.length chunk + 1) in
    Bytes.blit_string chunk 0 chunk' 0 (String.length chunk) ;
    check_end_of_body >>= choose chunk'

  let with_emitter ~emitter end_of_body =
    let write_line x = emitter (Some x) in
    parser ~write_line end_of_body

  let to_end_of_input ~write_data =
    fix @@ fun m ->
    peek_char >>= function
    | None -> commit
    | Some _ ->
        available >>= fun n ->
        Unsafe.take n (fun ba ~off ~len ->
            let chunk = Bytes.create len in
            Bigstringaf.blit_to_bytes ba ~src_off:off chunk ~dst_off:0 ~len ;
            write_data (Bytes.unsafe_to_string chunk))
        >>= fun () -> m
end

module QP = struct
  open Angstrom

  let parser ~write_data ~write_line end_of_body =
    let dec = Pecu.decoder `Manual in

    let check_end_of_body =
      let expected_len = String.length end_of_body in
      Unsafe.peek expected_len (fun ba ~off ~len ->
          let raw = Bigstringaf.substring ba ~off ~len in
          String.equal raw end_of_body) in

    let trailer () =
      let rec finish () =
        match Pecu.decode dec with
        | `Await -> assert false
        (* on [pecu], because [finish] was called just before [Pecu.src dec
           Bytes.empty 0 0] (so, when [len = 0]), semantically, it's impossible
           to retrieve this case. If [pecu] expects more inputs and we noticed
           end of input, it will return [`Malformed]. *)
        | `Data data ->
            write_data data ;
            finish ()
        | `Line line ->
            write_line line ;
            finish ()
        | `End -> commit
        | `Malformed err -> fail err
      and go () =
        match Pecu.decode dec with
        | `Await ->
            (* definitely [end_of_body]. *)
            Pecu.src dec Bytes.empty 0 0 ;
            finish ()
        | `Data data ->
            write_data data ;
            go ()
        | `Line line ->
            write_line line ;
            go ()
        | `End -> commit
        | `Malformed err -> fail err in

      go () in

    fix @@ fun m ->
    let choose chunk = function
      | true ->
          (* at this stage, we are at the end of body. We came from [`Await] case,
             so it's safe to notice to [pecu] the last [chunk]. [trailer] will
             unroll all outputs availables on [pecu]. *)
          let chunk = Bytes.sub chunk 0 (Bytes.length chunk - 1) in
          Pecu.src dec chunk 0 (Bytes.length chunk) ;
          trailer ()
      | false ->
          (* at this stage, byte after [chunk] is NOT a part of [end_of_body]. We
             can notice to [pecu] [chunk + end_of_body.[0]], advance on the
             Angstrom's input to one byte, and recall fixpoint until [`Await] case
             (see below). *)
          Bytes.set chunk (Bytes.length chunk - 1) end_of_body.[0] ;
          Pecu.src dec chunk 0 (Bytes.length chunk) ;
          advance 1 *> m in

    (* take while we did not discover the first byte of [end_of_body]. *)
    Unsafe.take_while (( <> ) end_of_body.[0]) Bigstringaf.substring
    >>= fun chunk ->
    (* start to know what we need to do with [pecu]. *)
    let rec go () =
      match Pecu.decode dec with
      | `End -> commit
      | `Await ->
          (* [pecu] expects inputs. At this stage, we know that after [chunk], we
             have the first byte of [end_of_body] - but we don't know if we have
             [end_of_body] or a part of it.

             [check_end_of_body] will advance to see if we really have
             [end_of_body]. The result will be sended to [choose]. *)
          let chunk' = Bytes.create (String.length chunk + 1) in
          Bytes.blit_string chunk 0 chunk' 0 (String.length chunk) ;
          check_end_of_body >>= choose chunk'
      | `Data data ->
          write_data data ;
          go ()
      | `Line line ->
          write_line line ;
          go ()
      | `Malformed err -> fail err in
    go ()

  let with_emitter ?(end_of_line = "\n") ~emitter end_of_body =
    let write_data x = emitter (Some x) in
    let write_line x = emitter (Some (x ^ end_of_line)) in
    parser ~write_data ~write_line end_of_body

  let to_end_of_input ~write_data ~write_line =
    let dec = Pecu.decoder `Manual in

    fix @@ fun m ->
    match Pecu.decode dec with
    | `End -> commit
    | `Await -> (
        peek_char >>= function
        | None ->
            Pecu.src dec Bytes.empty 0 0 ;
            return ()
        | Some _ ->
            available >>= fun n ->
            Unsafe.take n (fun ba ~off ~len ->
                let chunk = Bytes.create len in
                Bigstringaf.blit_to_bytes ba ~src_off:off chunk ~dst_off:0 ~len ;
                Pecu.src dec chunk 0 len)
            >>= fun () -> m)
    | `Data data ->
        write_data data ;
        m
    | `Line line ->
        write_line line ;
        m
    | `Malformed err -> fail err
end

type 'a elt = { header : Header.t; body : 'a }

type 'a t = Leaf of 'a elt | Multipart of 'a t option list elt

let rec map f = function
  | Leaf { header; body } -> Leaf { header; body = f body }
  | Multipart { header ; body } ->
    Multipart { header ; body = List.map (Option.map (map f)) body }

let iter ~f buf ~off ~len =
  for i = off to len - 1 do
    f buf.[i]
  done

let to_quoted_printable :
    ?length:int -> (string * int * int) stream -> (string * int * int) stream =
 fun ?length:(chunk_length = 4096) stream ->
  let chunk = Bytes.create chunk_length in
  let encoder = Pecu.encoder `Manual in
  let queue = Ke.Rke.create ~capacity:128 Bigarray.Int in

  let rec emit () =
    Ke.Rke.cons queue 256 ;
    let len = chunk_length - Pecu.dst_rem encoder in
    Some (Bytes.unsafe_to_string chunk, 0, len)
  and pending = function
    | `Ok -> go ()
    | `Partial ->
        let len = chunk_length - Pecu.dst_rem encoder in
        Some (Bytes.unsafe_to_string chunk, 0, len)
  and go () =
    match Ke.Rke.pop_exn queue with
    | 256 (* Await *) -> (
        Pecu.dst encoder chunk 0 chunk_length ;
        match Pecu.encode encoder `Await with
        | `Ok -> (go [@tailcall]) ()
        | `Partial -> (emit [@tailcall]) ())
    | 257 (* Line_break *) -> (
        (* XXX(dinosaure): we encode, in any case, a last CRLF to ensure that any
           line emitted by [to_quoted_printable] finish with a [CRLF]. TODO: may
           be this behavior is strictly under [Pecu] impl. *)
        Ke.Rke.cons queue 258 ;
        match Pecu.encode encoder `Line_break with
        | `Ok -> go ()
        | `Partial -> (emit [@tailcall]) ())
    | 258 (* End *) ->
        Ke.Rke.cons queue 259 ;
        (pending [@tailcall]) (Pecu.encode encoder `End)
    | 259 ->
        assert (Pecu.encode encoder `Await = `Ok) ;
        Ke.Rke.cons queue 259 ;
        None
    | chr -> (
        match Pecu.encode encoder (`Char (Char.chr chr)) with
        | `Ok -> (go [@tailcall]) ()
        | `Partial -> (emit [@tailcall]) ())
    | exception Ke.Rke.Empty ->
    match stream () with
    | Some (buf, off, len) ->
        iter ~f:(fun chr -> Ke.Rke.push queue (Char.code chr)) buf ~off ~len ;
        (go [@tailcall]) ()
    | None ->
        Ke.Rke.push queue 257 ;
        (go [@tailcall]) () in

  Pecu.dst encoder chunk 0 chunk_length ;
  go

let to_base64 :
    ?length:int -> (string * int * int) stream -> (string * int * int) stream =
 fun ?length:(chunk_length = 4096) stream ->
  let chunk = Bytes.create chunk_length in
  let encoder = Base64_rfc2045.encoder `Manual in
  let queue = Ke.Rke.create ~capacity:128 Bigarray.Int in

  let rec emit () =
    Ke.Rke.cons queue 256 ;
    let len = chunk_length - Base64_rfc2045.dst_rem encoder in
    Some (Bytes.unsafe_to_string chunk, 0, len)
  and pending = function
    | `Ok -> (go [@tailcall]) ()
    | `Partial ->
        let len = chunk_length - Base64_rfc2045.dst_rem encoder in
        Some (Bytes.unsafe_to_string chunk, 0, len)
  and go () =
    match Ke.Rke.pop_exn queue with
    | 256 (* Await *) -> (
        Base64_rfc2045.dst encoder chunk 0 chunk_length ;
        match Base64_rfc2045.encode encoder `Await with
        | `Ok -> (go [@tailcall]) ()
        | `Partial -> (emit [@tailcall]) ())
    | 257 (* End *) ->
        Ke.Rke.cons queue 258 ;
        (pending [@tailcall]) (Base64_rfc2045.encode encoder `End)
    | 258 ->
        assert (Base64_rfc2045.encode encoder `Await = `Ok) ;
        Ke.Rke.cons queue 258 ;
        None
    | chr -> (
        match Base64_rfc2045.encode encoder (`Char (Char.chr chr)) with
        | `Ok -> (go [@tailcall]) ()
        | `Partial -> (emit [@tailcall]) ())
    | exception Ke.Rke.Empty ->
    match stream () with
    | Some (buf, off, len) ->
        iter ~f:(fun chr -> Ke.Rke.push queue (Char.code chr)) buf ~off ~len ;
        (go [@tailcall]) ()
    | None ->
        Ke.Rke.push queue 257 ;
        (go [@tailcall]) () in

  Base64_rfc2045.dst encoder chunk 0 chunk_length ;
  go

let content_encoding fields =
  let encoding : Content_encoding.t ref = ref `Bit7 in
  let exception Found in
  try
    List.iter
      (function
        | Field.Field (_, Content_encoding, v) ->
            encoding := v ;
            raise Found
        | _ -> ())
      fields ;
    !encoding
  with Found -> !encoding

let failf fmt = Fmt.kstrf Angstrom.fail fmt

let octet ~emitter boundary header =
  let open Angstrom in
  match boundary with
  | None ->
      let write_line line = emitter (Some (line ^ "\n")) in
      let write_data data = emitter (Some data) in

      (match content_encoding header with
      | `Quoted_printable -> QP.to_end_of_input ~write_data ~write_line
      | `Base64 -> B64.to_end_of_input ~write_data
      | `Bit7 | `Bit8 | `Binary -> RAW.to_end_of_input ~write_data
      | `Ietf_token v | `X_token v ->
          failf "Invalid Content-Transfer-Encoding value (%s)" v)
      >>= fun () ->
      emitter None ;
      return ()
  | Some boundary ->
      let end_of_body = Rfc2046.make_delimiter boundary in
      (match content_encoding header with
      | `Quoted_printable -> QP.with_emitter ~emitter end_of_body
      | `Base64 -> B64.with_emitter ~emitter end_of_body
      | `Bit7 | `Bit8 | `Binary -> RAW.with_emitter ~emitter end_of_body
      | `Ietf_token v | `X_token v ->
          failf "Invalid Content-Transfer-Encoding value (%s)" v)
      >>= fun () ->
      emitter None ;
      return ()

type 'id emitters = Header.t -> (string option -> unit) * 'id

type discrete = [ `Text | `Image | `Audio | `Video | `Application ]

let boundary header =
  let content_type = Header.content_type header in
  match List.assoc_opt "boundary" (Content_type.parameters content_type) with
  | Some (Token boundary) | Some (String boundary) -> Some boundary
  | None -> None

let parser : emitters:'id emitters -> Field.field list -> 'id t Angstrom.t =
 fun ~emitters header ->
  let open Angstrom in
  let rec body parent header =
    match Content_type.ty (Header.content_type header) with
    | `Ietf_token v | `X_token v ->
        failf "Invalid Content-Transfer-Encoding value (%s)" v
    | #discrete ->
        let emitter, id = emitters header in
        octet ~emitter parent header >>| fun () -> Leaf { header; body = id }
    | `Multipart ->
    match boundary header with
    | Some boundary ->
        Rfc2046.multipart_body ?parent boundary (body (Option.some boundary))
        >>| List.map (fun (_header, contents) -> contents)
        >>| fun parts -> Multipart { header; body = parts }
    | None -> failf "Invalid Content-Type, missing boundary" in
  body None header

let parser ~emitters content_type =
  parser ~emitters
    [ Field.Field (Field_name.content_type, Field.Content_type, content_type) ]

let blit src src_off dst dst_off len =
  Bigstringaf.blit_from_string src ~src_off dst ~dst_off ~len

let of_stream stream content_type =
  let gen =
    let v = ref (-1) in
    fun () ->
      incr v ;
      !v in
  let tbl = Hashtbl.create 0x10 in
  let emitters _header =
    let idx = gen () in
    let buf = Buffer.create 0x100 in
    Hashtbl.add tbl idx buf ;
    ((function Some str -> Buffer.add_string buf str | None -> ()), idx) in
  let parser = parser ~emitters content_type in
  let module Ke = Ke.Rke in
  let ke = Ke.create ~capacity:0x1000 Bigarray.Char in
  let rec go = function
    | Angstrom.Unbuffered.Done (_, m) ->
        let assoc =
          Hashtbl.fold (fun k b a -> (k, Buffer.contents b) :: a) tbl [] in
        Ok (m, assoc)
    | Fail _ -> Error (`Msg "Invalid input")
    | Partial { committed; continue } -> (
        Ke.N.shift_exn ke committed ;
        if committed = 0 then Ke.compress ke ;
        match stream () with
        | Some str ->
            (* TODO: [""] *)
            Ke.N.push ke ~blit ~length:String.length ~off:0
              ~len:(String.length str) str ;
            let[@warning "-8"] (slice :: _) = Ke.N.peek ke in
            go
              (continue slice ~off:0 ~len:(Bigstringaf.length slice) Incomplete)
        | None ->
            let[@warning "-8"] (slice :: _) = Ke.N.peek ke in
            go (continue slice ~off:0 ~len:(Bigstringaf.length slice) Complete))
  in
  go (Angstrom.Unbuffered.parse parser)

let of_string str content_type =
  let consumed = ref false in
  let stream () =
    if !consumed
    then None
    else (
      consumed := true ;
      Some str) in
  of_stream stream content_type

type part = { header : Header.t; body : (string * int * int) stream }

type multipart = { header : Header.t; parts : part list }

let part ?(header = Header.empty) ?disposition ?encoding stream =
  let header =
    match disposition with
    | Some v ->
        Header.add Field_name.content_disposition
          (Field.Content_disposition, v)
          header
    | None -> header in
  let header =
    match encoding with
    | Some v ->
        Header.add Field_name.content_transfer_encoding
          (Field.Content_encoding, v)
          header
    | None -> header in
  let content_type = Header.content_type header in
  let encoding = content_encoding header in
  if not (Content_type.is_discrete content_type)
  then Fmt.invalid_arg "Content-type MUST be discrete type to a make a part" ;
  let stream =
    match encoding with
    | `Quoted_printable -> to_quoted_printable stream
    | `Base64 -> to_base64 stream
    | `Bit8 | `Binary | `Bit7 -> stream
    | `Ietf_token _ | `X_token _ -> assert false in
  { header; body = stream }

let multipart_content_default =
  let open Content_type in
  make `Multipart (Subtype.v "form-data") Parameters.empty

let multipart ~rng ?g ?(header = Header.empty) ?boundary parts =
  let boundary =
    match boundary with Some boundary -> boundary | None -> rng ?g 8 in
  let boundary = Content_type.Parameters.v boundary in
  let content_type =
    if Header.exists Field_name.content_type header
    then Header.content_type header
    else multipart_content_default in
  let content_type =
    Content_type.with_parameter content_type ("boundary", boundary) in
  let header =
    Header.replace Field_name.content_type
      (Field.Content_type, content_type)
      header in
  { header; parts }

(* stream helpers *)
module Stream = struct

  let none () = None

  let map f stream =
    let go () = match stream () with Some v -> Some (f v) | None -> None in
    go

  let of_string x =
    let once = ref false in
    let go () =
      if !once
      then None
      else (
        once := true ;
        Some (x, 0, String.length x)) in
    go

  let crlf () = of_string "\r\n"

  let concat s0 s1 =
    let c = ref s0 in
    let rec go () =
      match !c () with
      | Some x -> Some x
      | None ->
          if !c == s0
          then (
            c := s1 ;
            go ())
          else None in
    go

  let ( @ ) a b = concat a b

  let of_part { header; body } =
    let content_stream =
      map
        (fun s -> (s, 0, String.length s))
        (Prettym.to_stream Header.Encoder.header header) in
    content_stream @ crlf () @ body

end

let to_stream : multipart -> Header.t * (string * int * int) stream =
 fun { header; parts } ->
  let boundary =
    match Content_type.boundary (Header.content_type header) with
    | Some v -> v
    | None -> Fmt.failwith "Multipart MUST have a boundary"
    (* XXX(dinosaure): should never occur! *) in
  let beginner = Rfc2046.make_dash_boundary boundary ^ "\r\n" in
  let inner = Rfc2046.make_delimiter boundary ^ "\r\n" in
  let closer = Rfc2046.make_close_delimiter boundary ^ "\r\n" in

  let rec go stream = function
    | [] -> Stream.none
    | [ x ] -> Stream.(stream @ of_part x @ of_string closer)
    | x :: r ->
        let stream = Stream.(stream @ of_part x @ of_string inner) in
        go stream r in

  (header, go (Stream.of_string beginner) parts)
