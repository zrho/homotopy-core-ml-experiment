open Base

type generator = int
type t

val dimension : t -> int

(** [size diagram] is the number of slices of [diagram]. *)
val size : t -> int

(** {2 Matching} *)

(** [equal a b] determines whether the diagrams [a] and [b] are equal. *)
val equal : t -> t -> bool

(** [is_subdiagram embedding needle haystack] determines if the diagram [needle] is a
    subdiagram of [haystack] when embedded via the specified [embedding]. *)
val is_subdiagram : int list -> t -> t -> bool

(** [embeddings needle haystack] is a sequence of all possible ways the diagram [needle]
    can be embedded in [haystack]. *)
val embeddings : t -> t -> int list Sequence.t

(** {2 Slices} *)

(** [slices diagram] is a sequence of all the slices of [diagram]. The sequence is empty
    when [diagram] has dimension zero, and contains at least one slice otherwise. *)
val slices : t -> t Sequence.t

(** [singular_slices diagram] is a sequence of all the singular slices of [diagram],
    which are the slices of [diagram] at odd height. *)
val singular_slices : t -> t Sequence.t

(** [regular_slices diagram] is a sequence of all the regular slices of [diagram], which
    are the slices of [diagram] at even height. *)
val regular_slices : t -> t Sequence.t

(** [source_slice diagram] is the source slice of [diagram]. Returns [None] for
    0-dimensional diagrams. *)
val source_slice : t -> t option

(** [target_slice diagram] is the target slice of [diagram]. Returns [None] for
    0-dimensional diagrams. *)
val target_slice : t -> t option

(** [source_slice_exn diagram] is the source slice of [diagram]. Throws an exception for
    0-dimensional diagrams. *)
val source_slice_exn : t -> t

(** [source diagram] is the target slice of [diagram]. Throws an exception for
    0-dimensional diagrams. *)
val target_slice_exn : t -> t

(** {2 Construction} *)

(** [identity diagram] creates a diagram in which [diagram] is the only slice. *)
val identity : t -> t

(** [make ~boundary generator] creates a diagram for a single generator.

    When a [boundary] is given by a pair of diagrams [source] and [target] of dimension
    [n], the result is a diagram with dimension [n + 1] and regular slices [source] and
    [target]. An exception is raised if [source] and [target] are not globular.

    When no [boundary] is given, a 0-dimensional diagram is created. *)
val make : ?boundary:t * t -> generator -> t

val attach : ?boundary:[ `source | `target ] -> ?embed:int list -> t -> t -> t

(** {2 Heights} *)

type height =
  | Singular of int 
  | Regular of int

val height_of_int : int -> height
val int_of_height : height -> int

(** {2 Graph} *)

val points : t -> (height list * generator) Sequence.t
val edges : t -> (height list * height list) Sequence.t

(** {2 Miscellaneous} *)

val sexp_of_t : t -> Sexp.t