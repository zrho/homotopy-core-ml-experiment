open Base

type node = {
  id: int;
  mutable successors: int list;
}

type t = node Union_find.t Array.t

let make_linear n =
  Array.init n ~f:(fun id ->
      let successors =
        if id + 1 < n then
          [id + 1]
        else
          []
      in
      Union_find.make { id; successors })

let contract a b graph =
  Union_find.union
    (fun a_node b_node ->
      { id = a_node.id; successors = List.append a_node.successors b_node.successors })
    graph.(a)
    graph.(b)

let canonical_id graph v = (Union_find.find graph.(v)).id

let vertices graph =
  Array.to_sequence graph
  |> Sequence.mapi ~f:(fun id v -> (id, v))
  |> Sequence.filter ~f:(fun (id, v) -> Int.equal (Union_find.find v).id id)
  |> Sequence.map ~f:(fun (id, _) -> id)

let successors graph v =
  let node = Union_find.find graph.(v) in
  node.successors <-
    node.successors
    |> List.map ~f:(canonical_id graph)
    |> List.dedup_and_sort ~compare:Int.compare;
  node.successors |> Sequence.of_list

let sexp_of_t graph =
  let sexp_vertices = vertices graph |> Sequence.to_list |> [%sexp_of: int list] in
  let sexp_edges =
    vertices graph
    |> Sequence.bind ~f:(fun v ->
           successors graph v |> Sequence.map ~f:(fun w -> (v, w)))
    |> Sequence.to_list |> [%sexp_of: (int * int) list]
  in
  Sexp.List
    [
      Sexp.List [Sexp.Atom "vertices"; sexp_vertices];
      Sexp.List [Sexp.Atom "edges"; sexp_edges];
    ]

module Scc = struct
  let tarjan graph =
    let node_index = Array.create ~len:(Array.length graph) (-1) in
    let node_lowlink = Array.create ~len:(Array.length graph) (-1) in
    let node_onstack = Array.create ~len:(Array.length graph) false in
    let index = ref 0 in
    let stack = Stack.create () in
    let sccs = ref [] in
    let rec connect v =
      node_index.(v) <- !index;
      node_lowlink.(v) <- !index;
      index := !index + 1;
      Stack.push stack v;
      node_onstack.(v) <- true;
      Sequence.iter (successors graph v) ~f:(fun w ->
          if node_index.(w) < 0 then (
            connect w;
            node_lowlink.(v) <- Int.min node_lowlink.(v) node_lowlink.(w)
          ) else if node_onstack.(w) then
            node_lowlink.(v) <- Int.min node_lowlink.(v) node_index.(w));
      if Int.equal node_index.(v) node_lowlink.(v) then (
        let scc =
          Sequence.unfold ~init:() ~f:(fun _ ->
              Stack.pop stack
              |> Option.filter ~f:(fun w -> not (Int.equal v w))
              |> Option.map ~f:(fun w -> (w, ())))
          |> Sequence.to_list
        in
        let scc = v :: scc in
        List.iter scc ~f:(fun w -> node_onstack.(w) <- false);
        sccs := scc :: !sccs
      )
    in
    vertices graph |> Sequence.iter ~f:(fun v -> if node_index.(v) < 0 then connect v);
    !sccs
end

let contract_cycles graph =
  Scc.tarjan graph
  |> List.iter ~f:(fun scc ->
         ignore
           (List.reduce
              ~f:(fun a b ->
                contract a b graph;
                a)
              scc))
