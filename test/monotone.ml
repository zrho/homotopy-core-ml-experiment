open Base
open Stdio
module Monotone = Homotopy_core.Monotone

let%expect_test _ =
  let monotone = Monotone.make [1, 2; 3, 0] in

  List.init 5 ~f:(Monotone.image monotone)
  |> [%sexp_of: int list]
  |> Sexp.to_string
  |> print_endline;
  [%expect {| (0 1 1 3 4) |}];

  List.init 5 ~f:(Monotone.preimage monotone)
  |> [%sexp_of: int list list]
  |> Sexp.to_string
  |> print_endline;
  [%expect{| ((0)(1 2)()(3)(4)) |}];

  List.init 5 ~f:(Monotone.dual_image monotone)
  |> [%sexp_of: int list]
  |> Sexp.to_string
  |> print_endline;
  [%expect {| (0 1 3 3 4) |}];

  List.init 5 ~f:(Monotone.dual_preimage monotone)
  |> [%sexp_of: int list list]
  |> Sexp.to_string
  |> print_endline;
  [%expect {| ((0)(1)()(2 3)(4)) |}];
