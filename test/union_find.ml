open Base
open Stdio
module Union_find = Homotopy_core.Union_find

let%expect_test _ =
  let print_values sets =
    List.map ~f:Union_find.find sets
    |> [%sexp_of: int list list] |> Sexp.to_string |> print_endline
  in
  let a = Union_find.make [0] in
  let b = Union_find.make [1] in
  let c = Union_find.make [2] in
  print_values [a; b; c];
  [%expect {| ((0)(1)(2)) |}];

  Union_find.union List.append a b;
  print_values [a; b; c];
  [%expect {| ((0 1)(0 1)(2)) |}];

  Union_find.union List.append a c;
  print_values [a; b; c];
  [%expect {| ((0 1 2)(0 1 2)(0 1 2)) |}]
