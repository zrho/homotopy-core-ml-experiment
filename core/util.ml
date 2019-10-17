open Base

module List = struct
  let splice index remove insert list =
    let left, rest = List.split_n list index in
    let _, right = List.split_n rest remove in
    List.concat [left; insert; right]
end

module Sequence = struct
  let scan ~init ~f sequence =
    Sequence.shift_right
      (Sequence.folding_map
         ~init
         ~f:(fun s x ->
           let y = f s x in
           (y, y))
         sequence)
      init

  let enumerate = Sequence.mapi ~f:(fun i x -> (i, x))
end

module Counter = struct
  type t = int ref

  let make init = ref init

  let next counter =
    let value = !counter in
    counter := value + 1;
    value

  let skip counter n = counter := !counter + n
end

module Nat = struct
  type zero = Z
  type 'a succ = S of 'a
end
