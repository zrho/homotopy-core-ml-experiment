type 'a t

(** [make value] creates a new set. *)
val make : 'a -> 'a t

(** [find set] returns the value of the [set]. *)
val find : 'a t -> 'a

(** [union f a b] unifies the sets [a] and [b] and merges their values with [f]. *)
val union : ('a -> 'a -> 'a) -> 'a t -> 'a t -> unit
