open Base

type t

(** {2 Construction} *)

val make : (int * int) List.t -> t
val identity : t

(** {2 Primal} *)

val image : t -> int -> int
val preimage : t -> int -> int List.t

(** {2 Dual} *)

val dual_image : t -> int -> int
val dual_preimage : t -> int -> int List.t

(** {2 Colimits} *)

val colimit : (int * t) * (int * t) list -> t list

(** {2 Miscellaneous} *)

val sexp_of_t : t -> Sexp.t
