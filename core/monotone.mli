open Base

type t

val identity : t
val of_array : int Array.t -> t
val to_array : t -> int Array.t
val forward : t -> int -> int
val backward : t -> int -> int Sequence.t
val sexp_of_t : t -> Sexp.t