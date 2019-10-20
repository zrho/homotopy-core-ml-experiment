open Base

type part = {
  offset: int;
  size: int;
}

type t = part list

let identity = []

let make = List.map ~f:(fun (offset, size) -> { offset; size })

let image monotone index =
  let rec loop offset = function
    | [] -> index + offset
    | p::_ when index < p.offset -> index + offset
    | p::_ when index < p.offset + p.size -> p.offset + offset
    | p::ps -> loop (offset + 1 - p.size) ps
  in loop 0 monotone

let dual_image monotone index =
  let rec loop index = function
    | [] -> index 
    | p::_ when index <= p.offset -> index 
    | p::ps -> loop (index - 1 + p.size) ps
  in loop index monotone

let naive_preimage ~f value = 
  Sequence.unfold ~init:0 ~f:(fun i -> Some ((i, f i), i + 1))
    |> Sequence.take_while ~f:(fun (_, v) -> v <= value)
    |> Sequence.filter ~f:(fun (_, v) -> v = value)
    |> Sequence.map ~f:(fun (i, _) -> i)
    |> Sequence.to_list

(* TODO: Make these efficient *)
let preimage monotone = naive_preimage ~f:(image monotone)
let dual_preimage monotone = naive_preimage ~f:(dual_image monotone)

let sexp_of_t monotone =
  monotone
    |> List.map ~f:(fun part -> [part.offset; part.size])
    |> [%sexp_of: int list list]

let colimit _ = assert false

(*
module Graph: sig
  type t
  val contract : t -> int -> int -> unit
  val succ : t -> int -> int Sequence.t
  val nodes : t -> int Sequence.t
  val contract_cycles : t -> unit
end = struct
  type node = {
    node_id: int;
    node_succ: int list;
  }

  type t = node Union_find.t Array.t

  let contract graph a b =
    Union_find.union (fun a b ->
      { node_id = a.node_id; node_succ = List.append a.node_succ b.node_succ })
      graph.(a) graph.(b)

  let repr graph id = (Union_find.find graph.(id)).node_id

  let succ graph v =
    let node = Union_find.find graph.(v) in
    Sequence.of_list node.node_succ
    |> Sequence.filter ~f:(fun id -> repr graph id = id)

  let nodes graph =
    Sequence.init (Array.length graph) ~f:(fun id -> id)
    |> Sequence.filter ~f:(fun id -> id = repr graph id)

  let contract_cycles graph =
    let node_count = Array.length graph in
    let indices = Array.create ~len:node_count (-1) in
    let lowlinks = Array.create ~len:node_count (-1) in
    let onstack = Array.create ~len:node_count false in
    let index = ref 0 in
    let stack = Stack.create () in

    let rec connect v =
      indices.(v) <- !index;
      lowlinks.(v) <- !index;
      index := !index + 1;
      Stack.push stack v;
      onstack.(v) <- true;

      Sequence.iter (succ graph v) ~f:(fun w ->
        if indices.(w) < 0 then (
          connect w;
          lowlinks.(v) <- Int.min lowlinks.(v) lowlinks.(w)
        ) else if onstack.(w) then (
          lowlinks.(v) <- Int.min lowlinks.(v) indices.(w)
        )
      );

      if indices.(v) = lowlinks.(v) then (
        let rec loop () =
          match Stack.pop stack with
          | Some(w) ->
              onstack.(w) <- false;
              if not (v = w) then (
                contract graph v w;
                loop ()
              )
          | _ -> ()
        in loop ()
      )
    in
    Sequence.iter (nodes graph) ~f:(fun v -> if indices.(v) < 0 then connect v)
end
*)
