open Base
open Stdio
module Diagram = Homotopy_core.Diagram

let print_svg ~colors diagram = 
  let height_to_coord height = (Diagram.int_of_height height) * 50 + 100 in
  print_endline {|<?xml version="1.0" encoding="UTF-8" ?>|};
  print_endline {|<svg xmlns="http://www.w3.org/2000/svg">|};
  print_endline {|<marker id="arrow" viewBox="0 0 10 10" refX="5" refY="5"
        markerWidth="6" markerHeight="6"
        orient="auto-start-reverse">
      <path d="M 0 0 L 5 5 L 0 10 z" />
    </marker>|};
  Sequence.iter (Diagram.points diagram) ~f:(function
    | [y; x], generator ->
      let x = height_to_coord x in
      let y = height_to_coord y in
      let c = colors generator in
      printf "  <circle cx=\"%i\" cy=\"%i\" fill=\"%s\" r=\"5\" />\n" x y c
    | _, _ -> assert false
  );
  Sequence.iter (Diagram.edges diagram) ~f:(function
    | [sy; sx], [ty; tx] ->
      let sx = height_to_coord sx in
      let sy = height_to_coord sy in
      let tx = height_to_coord tx in
      let ty = height_to_coord ty in
      printf "  <path d=\"M %i %i L %i %i\" stroke=\"black\" marker-end=\"url(#arrow)\" />\n" sx sy tx ty
    | _, _ -> assert false
  );
  print_endline {|</svg>|}

let () =
  let a = Diagram.make 1 in
  let f = Diagram.make ~boundary:(a, a) 2 in
  let ff = Diagram.attach f f in
  let t = Diagram.make ~boundary:(ff, f) 3 in
  (*let tf = Diagram.attach ~boundary:`target t f in*)
  (*let left = Diagram.attach tf t in*)
  let ft = Diagram.attach ~boundary:`source t f in
  (*let right = Diagram.attach ft t in
  let assoc = Diagram.make ~boundary:(left, right) 4 in*)
  print_svg ft ~colors:(fun generator ->
    [| "red"; "green"; "blue"; "cyan"; "yellow" |].(generator))