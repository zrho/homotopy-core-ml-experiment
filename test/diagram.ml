open Base
open Stdio
module Diagram = Homotopy_core.Diagram

let point_to_ints = List.map ~f:Diagram.int_of_height

let print_points diagram =
  Diagram.points diagram
  |> Sequence.map ~f:(fun (p, g) -> (point_to_ints p, g))
  |> Sequence.to_list
  |> [%sexp_of: (int list * int) list]
  |> Sexp.to_string |> print_endline

let print_edges diagram =
  Diagram.edges diagram
  |> Sequence.map ~f:(fun (s, t) -> (point_to_ints s, point_to_ints t))
  |> Sequence.to_list
  |> [%sexp_of: (int list * int list) list]
  |> Sexp.to_string |> print_endline

let%expect_test _ =
  let module Monotone = Homotopy_core.Monotone in
  let m = Monotone.of_array [|0; 0|] in
  Monotone.forward m 2 |> [%sexp_of: int] |> Sexp.to_string |> print_endline;
  [%expect]

let%expect_test _ =
  let a = Diagram.make 1 in
  let f = Diagram.make ~boundary:(a, a) 2 in
  let ff = Diagram.attach f f in
  let t = Diagram.make ~boundary:(ff, f) 3 in
  let tf = Diagram.attach ~boundary:`target t f in
  let left = Diagram.attach tf t in
  let ft = Diagram.attach ~boundary:`source t f in
  let right = Diagram.attach ft t in
  let assoc = Diagram.make ~boundary:(left, right) 4 in

  (* Left associated multiplication. *)
  print_points left;
  [%expect {|(((0 0)1)((0 1)2)((0 2)1)((0 3)2)((0 4)1)((0 5)2)((0 6)1)((1 0)1)((1 1)3)((1 2)1)((1 3)2)((1 4)1)((2 0)1)((2 1)2)((2 2)1)((2 3)2)((2 4)1)((3 0)1)((3 1)3)((3 2)1)((4 0)1)((4 1)2)((4 2)1)) |}];

  (* Right associated multiplication. *)
  print_points right;
  [%expect {|(((0 0)1)((0 1)2)((0 2)1)((0 3)2)((0 4)1)((0 5)2)((0 6)1)((1 0)1)((1 1)2)((1 2)1)((1 3)3)((1 4)1)((2 0)1)((2 1)2)((2 2)1)((2 3)2)((2 4)1)((3 0)1)((3 1)3)((3 2)1)((4 0)1)((4 1)2)((4 2)1)) |}];

  (* Associator from left to right. *)
  print_points assoc;
  [%expect {|(((0 0 0)1)((0 0 1)2)((0 0 2)1)((0 0 3)2)((0 0 4)1)((0 0 5)2)((0 0 6)1)((0 1 0)1)((0 1 1)3)((0 1 2)1)((0 1 3)2)((0 1 4)1)((0 2 0)1)((0 2 1)2)((0 2 2)1)((0 2 3)2)((0 2 4)1)((0 3 0)1)((0 3 1)3)((0 3 2)1)((0 4 0)1)((0 4 1)2)((0 4 2)1)((1 0 0)1)((1 0 1)2)((1 0 2)1)((1 0 3)2)((1 0 4)1)((1 0 5)2)((1 0 6)1)((1 1 0)1)((1 1 1)4)((1 1 2)1)((1 2 0)1)((1 2 1)2)((1 2 2)1)((2 0 0)1)((2 0 1)2)((2 0 2)1)((2 0 3)2)((2 0 4)1)((2 0 5)2)((2 0 6)1)((2 1 0)1)((2 1 1)2)((2 1 2)1)((2 1 3)3)((2 1 4)1)((2 2 0)1)((2 2 1)2)((2 2 2)1)((2 2 3)2)((2 2 4)1)((2 3 0)1)((2 3 1)3)((2 3 2)1)((2 4 0)1)((2 4 1)2)((2 4 2)1)) |}]

let print_exception ~f =
  try
    ignore (f ());
    print_endline "Success"
  with _ -> print_endline "Error"

let%expect_test _ =
  let a = Diagram.make 1 in
  let b = Diagram.make 2 in
  let c = Diagram.make 3 in
  let f = Diagram.make ~boundary:(a, b) 4 in
  let g = Diagram.make ~boundary:(b, c) 5 in

  (* Can't attach higher-dimensional diagrams to lower dimensional ones. *)
  print_exception ~f:(fun () -> Diagram.attach a f);
  [%expect {| Error |}];
  print_exception ~f:(fun () -> Diagram.attach a g);
  [%expect {| Error |}];

  (* Can't compose incompatible 1-dimensional diagrams. *)
  print_exception ~f:(fun () -> Diagram.attach g f);
  [%expect {| Error |}]

(*
     let a = Diagram.make_exn 1

     let f = Diagram.make_exn ~boundary:(a, a) 10

     let g = Diagram.make_exn ~boundary:(a, a) 20

     let h = Diagram.make_exn ~boundary:(a, a) 30

     let compose = Diagram.make_exn ~boundary:(Diagram.attach_exn f f, f) 100

     let whisker = Diagram.attach_exn ~boundary:`target ~depth:1 compose f

     let diagram = Diagram.attach_exn ~boundary:`target whisker compose

     let colors =
     Map.of_alist_exn
     (module Int)
     [ (1, "red"); (10, "blue"); (20, "green"); (30, "cyan"); (100, "pink") ]*)

(*
     let _ = print_endline (Diagram.sexp_of_t diagram |> Sexp.to_string)

     let _ =
     print_endline
     ( Diagram.monotone_test diagram
     |> List.sexp_of_t (List.sexp_of_t Int.sexp_of_t)
     |> Sexp.to_string )*)
(*
     let _ =
     print_endline
     "<?xml version=\"1.0\" encoding=\"UTF-8\" ?><svg \
     xmlns=\"http://www.w3.org/2000/svg\">";
     Diagram.points diagram
     |> Sequence.iter ~f:(function
     | [ y; x ], gen ->
     print_string "<circle cx=\"";
     print_string (Int.to_string ((x * 20) + 100));
     print_string "\" cy=\"";
     print_string (Int.to_string ((y * 20) + 100));
     print_string "\" fill=\"";
     print_string (Map.find_exn colors gen);
     print_endline "\" r=\"5\" />"
     | _ -> assert false);
     print_endline "</svg>"*)
