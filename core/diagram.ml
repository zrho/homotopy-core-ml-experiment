open Base

type generator = int [@@deriving sexp_of]

type height =
  | Singular of int 
  | Regular of int

type limit =
  | Limit0 of generator * generator
  | LimitN of cone list

and cone = {
  offset: int;
  source: cospan list;
  target: cospan;
  slices: limit list;
}

and cospan = limit * limit

and t =
  | Diagram0 of generator
  | DiagramN of t * cospan list
[@@deriving sexp_of]

let rec dimension = function
  | Diagram0 _ -> 0
  | DiagramN (source, _) -> dimension source + 1

let identity diagram = DiagramN (diagram, [])

let size = function
  | Diagram0 _ -> 1
  | DiagramN (_, cospans) -> List.length cospans

(****************************************************************************************)

let rewrite_forward limit diagram =
  match (limit, diagram) with
  | Limit0 (_, generator), Diagram0 _ -> Diagram0 generator
  | LimitN cones, DiagramN (source, content) ->
    let content, _ =
      List.fold_left cones ~init:(content, 0) ~f:(fun (content, offset) cone ->
          let source_len = List.length cone.source in
          ( Util.List.splice (cone.offset + offset) source_len [cone.target] content,
            offset - source_len + 1 ))
    in
    DiagramN (source, content)
  | Limit0 (_, _), DiagramN (_, _)
  | LimitN _, Diagram0 _ ->
    assert false

let rewrite_backward limit diagram =
  match (limit, diagram) with
  | Limit0 (generator, _), Diagram0 _ -> Diagram0 generator
  | LimitN cones, DiagramN (source, content) ->
    let content, _ =
      List.fold_left cones ~init:(content, 0) ~f:(fun (content, offset) cone ->
          let source_len = List.length cone.source in
          ( Util.List.splice (cone.offset + offset) 1 cone.source content,
            offset + source_len - 1 ))
    in
    DiagramN (source, content)
  | Limit0 (_, _), DiagramN (_, _)
  | LimitN _, Diagram0 _ ->
    assert false

(****************************************************************************************)

let slices = function
  | Diagram0 _ -> Sequence.empty
  | DiagramN (source, cospans) ->
    Sequence.of_list cospans
    |> Sequence.bind ~f:(fun (forward, backward) ->
           Sequence.of_list [rewrite_forward forward; rewrite_backward backward])
    |> Util.Sequence.scan ~init:source ~f:(fun s r -> r s)

let singular_slices diagram =
  slices diagram
  |> (fun s -> Sequence.take s (size diagram * 2))
  |> Sequence.filteri ~f:(fun i _ -> Int.equal (i % 2) 1)

let regular_slices diagram =
  slices diagram |> Sequence.filteri ~f:(fun i _ -> Int.equal (i % 2) 0)

let source_slice = function
  | DiagramN (source, _) -> Some source
  | Diagram0 _ -> None

let target_slice diagram = Sequence.reduce ~f:(fun _ x -> x) (slices diagram)
let source_slice_exn diagram = Option.value_exn (source_slice diagram)
let target_slice_exn diagram = Option.value_exn (target_slice diagram)

(****************************************************************************************)

let rec pad_cospan pad (forward, backward) =
  (pad_limit pad forward, pad_limit pad backward)

and pad_limit pad limit =
  match limit with
  | LimitN cones -> LimitN (List.map ~f:(pad_cone pad) cones)
  | Limit0 (_, _) -> limit

and pad_cone pad cone =
  match pad with
  | [] -> cone
  | p :: ps ->
    {
      offset = cone.offset + p;
      source = List.map ~f:(pad_cospan ps) cone.source;
      target = pad_cospan ps cone.target;
      slices = List.map ~f:(pad_limit ps) cone.slices;
    }

(****************************************************************************************)

let rec equal_cospan (f0, b0) (f1, b1) = equal_limit f0 f1 && equal_limit b0 b1

and equal_limit limit0 limit1 =
  match (limit0, limit1) with
  | Limit0 (s0, t0), Limit0 (s1, t1) -> Int.equal s0 s1 && Int.equal t0 t1
  | LimitN cones0, LimitN cones1 -> List.equal equal_cone cones0 cones1
  | Limit0 _, LimitN _
  | LimitN _, Limit0 _ ->
    false

and equal_cone cone0 cone1 =
  Int.equal cone0.offset cone1.offset
  && List.equal equal_cospan cone0.source cone1.source
  && equal_cospan cone0.target cone1.target
  && List.equal equal_limit cone0.slices cone1.slices

let rec equal a b =
  match (a, b) with
  | Diagram0 a, Diagram0 b -> Int.equal a b
  | DiagramN (a_s, a_c), DiagramN (b_s, b_c) ->
    equal a_s b_s && List.equal equal_cospan a_c b_c
  | Diagram0 _, DiagramN (_, _)
  | DiagramN (_, _), Diagram0 _ ->
    false

(****************************************************************************************)

let is_subdiagram _ _ _ = assert false
let embeddings _ = assert false
let reverse_cospan (a, b) = (b, a)

(****************************************************************************************)

let rec attach ?(boundary = `target) ?(embed = []) large small =
  assert (dimension small <= dimension large);
  let depth = dimension large - dimension small in
  match (depth, boundary, small, large) with
  | 0, `source, DiagramN (_, sc), DiagramN (ls, lc) ->
    let cospans = List.map ~f:(pad_cospan embed) sc in
    let reversed_cospans = cospans |> List.rev |> List.map ~f:reverse_cospan in
    let result_source = target_slice_exn (DiagramN (ls, reversed_cospans)) in
    let result_cospans = List.append cospans lc in
    DiagramN (result_source, result_cospans)
  | 0, `target, DiagramN (_, sc), DiagramN (ls, lc) ->
    let cospans = List.map ~f:(pad_cospan embed) sc in
    let result_cospans = List.append lc cospans in
    DiagramN (ls, result_cospans)
  | _, `source, _, DiagramN (ls, lc) ->
    let pad = List.append (List.init (depth - 1) ~f:(fun _ -> 0)) [size small] in
    let result_cospans = List.map ~f:(pad_cospan pad) lc in
    let result_source = attach ~boundary ~embed ls small in
    DiagramN (result_source, result_cospans)
  | _, `target, _, DiagramN (ls, lc) ->
    let result_source = attach ~boundary ~embed ls small in
    DiagramN (result_source, lc)
  | _, _, _, _ -> assert false

(****************************************************************************************)

let height_of_int index =
  if index % 2 = 1 then
    Singular ((index - 1) / 2)
  else
    Regular (index / 2)

let int_of_height = function
  | Regular height -> height * 2
  | Singular height -> height * 2 + 1

(****************************************************************************************)

let rec points = function
  | Diagram0 generator -> Sequence.singleton ([], generator)
  | DiagramN _ as diagram ->
    Sequence.concat_mapi (slices diagram) ~f:(fun index slice ->
      Sequence.map (points slice) ~f:(fun (rest, generator) -> 
        (height_of_int index :: rest, generator)))

let monotone_of_limit = function
  | Limit0 _ -> Monotone.identity
  | LimitN cones ->
      Monotone.make (List.map cones ~f:(fun cone ->
        (cone.offset, List.length cone.source)
      ))

let cone_slice height cone = List.nth cone.slices (height - cone.offset)

let limit_slice limit singular_height =
  match limit with
  | Limit0 _ -> None
  | LimitN cones -> List.find_map ~f:(cone_slice singular_height) cones

let rec limit_action limit = function
  | [] -> Sequence.singleton []
  | Regular height :: rest ->
    Monotone.dual_preimage (monotone_of_limit limit) height
    |> Sequence.of_list
    |> Sequence.map ~f:(fun height -> Regular height :: rest)
  | Singular height :: rest ->
    let rest = limit_action_option (limit_slice limit height) rest in
    let height = Monotone.image (monotone_of_limit limit) height in
    Sequence.map ~f:(fun rest -> Singular height :: rest) rest
and limit_action_option limit point = match limit with
  | None -> Sequence.singleton point
  | Some limit -> limit_action limit point

(* singular <- regular -> singular *)

let rec edges = function
  | Diagram0 _ -> Sequence.empty
  | DiagramN (_, cospans) as diagram ->
    let open Sequence.Let_syntax in
    let horizontal = (
      let%bind (i, slice) = Util.Sequence.enumerate (slices diagram) in
      let height = height_of_int i in
      let%map (s, t) = edges slice in
      (height :: s, height :: t)
    ) in
    let vertical = (
      let%bind (index, slice) = Util.Sequence.enumerate (regular_slices diagram) in
      let backward = Option.map ~f:(fun (_, limit) -> limit) (List.nth cospans (index - 1)) in
      let forward = Option.map ~f:(fun (limit, _) -> limit) (List.nth cospans index) in
      Sequence.append
        (
          let%bind (point, _) = points slice in
          let%map target = limit_action_option backward point in
          (Regular index :: point, Singular (index - 1) :: target)
        )
        (
          let%bind (point, _) = points slice in
          let%map target = limit_action_option forward point in
          (Regular index :: point, Singular index :: target)
        )
    ) in
    Sequence.append horizontal vertical

(****************************************************************************************)

let rec make_limit generator base =
  match base with
  | Diagram0 base -> Limit0 (base, generator)
  | DiagramN (source, cospans) ->
    let forward = make_limit generator source in
    let backward = make_limit generator (target_slice_exn base) in
    let target = (forward, backward) in
    let slices =
      singular_slices base |> Sequence.map ~f:(make_limit generator) |> Sequence.to_list
    in
    LimitN [{ offset = 0; source = cospans; target; slices }]

let make ?boundary generator =
  match boundary with
  | Some (source, target) ->
    assert (Option.equal equal (source_slice source) (source_slice target));
    assert (Option.equal equal (target_slice source) (target_slice target));
    DiagramN (source, [(make_limit generator source, make_limit generator target)])
  | None -> Diagram0 generator
