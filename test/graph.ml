open Base
open Stdio
module Graph = Homotopy_core.Graph

let print_graph graph = graph |> Graph.sexp_of_t |> Sexp.to_string_hum |> print_endline

let%expect_test _ =
  let graph = Graph.make_linear 10 in
  print_graph graph;
  [%expect
    {|
    ((vertices (0 1 2 3 4 5 6 7 8 9))
     (edges ((0 1) (1 2) (2 3) (3 4) (4 5) (5 6) (6 7) (7 8) (8 9)))) |}];

  Graph.contract 2 6 graph;
  print_graph graph;
  [%expect
    {|
    ((vertices (0 1 2 3 4 5 7 8 9))
     (edges ((0 1) (1 2) (2 3) (2 7) (3 4) (4 5) (5 2) (7 8) (8 9)))) |}];

  Graph.contract 4 8 graph;
  print_graph graph;
  [%expect
    {|
    ((vertices (0 1 2 3 4 5 7 9))
     (edges ((0 1) (1 2) (2 3) (2 7) (3 4) (4 5) (4 9) (5 2) (7 4)))) |}];

  Graph.contract_cycles graph;
  print_graph graph;
  [%expect {| ((vertices (0 1 2 9)) (edges ((0 1) (1 2) (2 2) (2 9)))) |}]
