open Common
open Printf

type numpad = N0 | N1 | N2 | N3 | N4 | N5 | N6 | N7 | N8 | N9 | NA
type dirpad = Up | Down | Left | Right | DA

let string_of_numpad = function
  | N0 -> "0"
  | N1 -> "1"
  | N2 -> "2"
  | N3 -> "3"
  | N4 -> "4"
  | N5 -> "5"
  | N6 -> "6"
  | N7 -> "7"
  | N8 -> "8"
  | N9 -> "9"
  | NA -> "A"

let string_of_dirpad = function
  | Up -> "^"
  | Down -> "v"
  | Left -> "<"
  | Right -> ">"
  | DA -> "A"

module NumpadEdge = struct
  type t = numpad * dirpad

  let compare = compare
end

module Numpad = struct
  type t = numpad

  let compare = compare
end

module NumpadGraph = Map.Make (NumpadEdge)
module NumpadMap = Map.Make (Numpad)
module NumpadSet = Set.Make (Numpad)

module DirpadEdge = struct
  type t = dirpad * dirpad

  let compare = compare
end

module Dirpad = struct
  type t = dirpad

  let compare = compare
end

module DirpadGraph = Map.Make (DirpadEdge)
module DirpadMap = Map.Make (Dirpad)
module DirpadSet = Set.Make (Dirpad)

(*
    +---+---+
    | ^ | A |
+---+---+---+
| < | v | > |
+---+---+---+ 
*)

let dirpad_graph =
  DirpadGraph.(
    empty
    |> add (DA, Left) Up
    |> add (DA, Down) Right
    |> add (Up, Right) DA
    |> add (Up, Down) Down
    |> add (Left, Right) Down
    |> add (Down, Left) Left
    |> add (Down, Up) Up
    |> add (Down, Right) Right
    |> add (Right, Left) Down
    |> add (Right, Up) DA)

(*
+---+---+---+
| 7 | 8 | 9 |
+---+---+---+
| 4 | 5 | 6 |
+---+---+---+
| 1 | 2 | 3 |
+---+---+---+
    | 0 | A |
    +---+---+
 *)

let numpad_graph =
  NumpadGraph.(
    empty
    |> add (N7, Right) N8
    |> add (N7, Down) N4
    |> add (N8, Left) N7
    |> add (N8, Right) N9
    |> add (N8, Down) N5
    |> add (N9, Left) N8
    |> add (N9, Down) N6
    |> add (N4, Up) N7
    |> add (N4, Right) N5
    |> add (N4, Down) N1
    |> add (N5, Up) N8
    |> add (N5, Left) N4
    |> add (N5, Right) N6
    |> add (N5, Down) N2
    |> add (N6, Up) N9
    |> add (N6, Left) N5
    |> add (N6, Down) N3
    |> add (N1, Up) N4
    |> add (N1, Right) N2
    |> add (N2, Up) N5
    |> add (N2, Left) N1
    |> add (N2, Right) N3
    |> add (N2, Down) N0
    |> add (N3, Up) N6
    |> add (N3, Left) N2
    |> add (N3, Down) NA
    |> add (N0, Up) N2
    |> add (N0, Right) NA
    |> add (NA, Up) N3
    |> add (NA, Left) N0)

module type Bfs = sig
  type element

  module VisitedSet : Set.S with type elt = element
  module Graph : Map.S with type key = element * dirpad
  module Parents : Map.S with type key = element

  val graph : element Graph.t
end

module MakeBfs (B : Bfs) : sig
  include Bfs

  val bfs : B.element -> B.element -> B.element list option

  (* val many_bfs : B.element list -> B.element list option *)
  val many_bfs : B.element list -> dirpad list option
end = struct
  type element = B.element

  module VisitedSet = B.VisitedSet
  module Graph = B.Graph

  module DirGraph = Map.Make (struct
    type t = B.element * B.element

    let compare = compare
  end)

  module Parents = B.Parents

  let graph = B.graph

  let dir_graph =
    Graph.to_list graph
    |> List.map (fun ((el1, dir), el2) -> ((el1, el2), dir))
    |> DirGraph.of_list

  let bfs src dst =
    let rec bfs visited parents = function
      | [] -> None
      | p :: q when VisitedSet.mem p visited -> bfs visited parents q
      | num :: q ->
          if num = dst then Some parents
          else
            let neighbors =
              [ Up; Down; Left; Right ]
              |> List.filter_map (fun d -> Graph.find_opt (num, d) graph)
              |> List.filter (fun num -> not @@ VisitedSet.mem num visited)
            in
            let new_visited = VisitedSet.add num visited in
            let new_parents =
              neighbors
              |> List.fold_left (fun acc n -> Parents.add n num acc) parents
            in
            let new_queue = q @ neighbors in
            bfs new_visited new_parents new_queue
    in

    let rec traverse p q parents =
      if p = q then []
      else
        let next = Parents.find p parents in
        next :: traverse next q parents
    in

    bfs VisitedSet.empty Parents.empty [ src ]
    |> Option.map (traverse dst src)
    |> Option.map (fun p -> dst :: p |> List.rev)

  let rec pairs = function
    | x1 :: x2 :: xs -> (x1, x2) :: pairs (x2 :: xs)
    | _ -> []

  (* let many_bfs steps = *)
  (*   steps |> pairs *)
  (*   |> List.map (fun (src, dst) -> bfs src dst) *)
  (*   |> List.fold_left *)
  (*        (fun acc segment -> *)
  (*          match (acc, segment) with *)
  (*          | Some acc, Some segment -> Some (acc @ List.tl segment) *)
  (*          | _ -> None) *)
  (*        (Some []) *)

  let many_bfs steps =
    let segments = steps |> pairs |> List.map (fun (src, dst) -> bfs src dst) in
    if List.for_all Option.is_some segments then
      Some
        (segments
        |> List.map (fun segment ->
               segment |> Option.get |> pairs
               |> List.map (fun k -> DirGraph.find k dir_graph))
        |> List.fold_left (fun acc seg -> (DA :: seg) @ acc) []
        |> List.rev)
    else None
end

module NumpadBfs = MakeBfs (struct
  type element = numpad

  module VisitedSet = NumpadSet
  module Graph = NumpadGraph
  module Parents = NumpadMap

  let graph = numpad_graph
end)

module DirpadBfs = MakeBfs (struct
  type element = dirpad

  module VisitedSet = DirpadSet
  module Graph = DirpadGraph
  module Parents = DirpadMap

  let graph = dirpad_graph
end)

let _ =
  NumpadBfs.many_bfs [ NA; N0; N2; N9; NA ]
  |> Option.get
  |> List.iter (fun d -> printf "%s " (string_of_dirpad d))

let _ = printf "\n"
