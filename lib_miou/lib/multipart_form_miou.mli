open Multipart_form

module Bounded_stream : sig
  type 'a t

  val create : int -> 'a -> 'a t
  val put : 'a t -> 'a option -> unit
  val get : 'a t -> 'a option
  val iter : ('a -> unit) -> 'a t -> unit
end

val stream :
  ?bounds:int ->
  identify:(Header.t -> 'id) ->
  epsilon:'id ->
  string Bounded_stream.t ->
  Content_type.t ->
  [ `Parse of ('id t, [> `Msg of string ]) result Miou.t ]
  * ('id * Header.t * string Bounded_stream.t) Bounded_stream.t

val of_stream_to_list :
  string Bounded_stream.t ->
  Content_type.t ->
  (int t * (int * string) list, [> `Msg of string ]) result
