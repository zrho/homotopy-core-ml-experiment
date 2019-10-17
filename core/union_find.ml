open Base

type 'a node =
  | Root of 'a * int
  | Link of 'a node ref

type 'a t = 'a node ref

let make value = ref (Root (value, 0))

let rec find_internal set =
  match !set with
  | Root (value, rank) -> (set, value, rank)
  | Link parent ->
    let root, value, rank = find_internal parent in
    set := Link root;
    (root, value, rank)

let find set =
  let _, value, _ = find_internal set in
  value

let union f a b =
  let a, a_value, a_rank = find_internal a in
  let b, b_value, b_rank = find_internal b in
  if phys_equal a b then
    ()
  else if a_rank > b_rank then (
    b := Link a;
    a := Root (f a_value b_value, a_rank)
  ) else if a_rank < b_rank then (
    a := Link b;
    b := Root (f b_value a_value, b_rank)
  ) else (
    b := Link a;
    a := Root (f a_value b_value, a_rank + 1)
  )
