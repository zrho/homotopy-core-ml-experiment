open Base

type t = int array
[@@deriving sexp_of]

let identity = [||]

let of_array arr =
  assert (Array.is_sorted ~compare:Int.compare arr);
  arr

let to_array monotone = monotone

let forward monotone index =
  if index < 0 then
    index
  else if index >= Array.length monotone then
    index - Array.length monotone + Array.last monotone + 1
  else
    monotone.(index)

let backward monotone value =
  if value < 0 then
    Sequence.singleton value
  else if value > Array.last monotone then
    Sequence.singleton (value - Array.last monotone + Array.length monotone - 1)
  else
    match Array.binary_search monotone ~compare:Int.compare `First_equal_to value with
    | Some index ->
      Sequence.range index (Array.length monotone)
      |> Sequence.take_while ~f:(fun index -> monotone.(index) = value)
    | None -> Sequence.empty
