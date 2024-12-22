open Common
open Printf
open Scanf

let test_case =
  {|
029A
980A
179A
456A
379A
|} |> String.trim |> String.split_on_char '\n'

let day_input = read_day_lines 21

type numpad = N0 | N1 | N2 | N3 | N4 | N5 | N6 | N7 | N8 | N9 | NA
type dirpad = Up | Left | Down | Right | DA

let numpad_of_char = function
  | '0' -> N0
  | '1' -> N1
  | '2' -> N2
  | '3' -> N3
  | '4' -> N4
  | '5' -> N5
  | '6' -> N6
  | '7' -> N7
  | '8' -> N8
  | '9' -> N9
  | 'A' -> NA
  | _ -> raise Unreachable

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

let parse =
  List.map (fun line ->
      let numpads =
        String.to_seq line |> Seq.map numpad_of_char |> List.of_seq
      in
      let number = sscanf line "%03dA" (fun x -> x) in
      (numpads, number))

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
  DirpadGraph.of_list
    [
      ((DA, Left), Up);
      ((DA, Down), Right);
      ((Up, Right), DA);
      ((Up, Down), Down);
      ((Left, Right), Down);
      ((Down, Left), Left);
      ((Down, Up), Up);
      ((Down, Right), Right);
      ((Right, Left), Down);
      ((Right, Up), DA);
    ]

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
  NumpadGraph.of_list
    [
      ((N7, Right), N8);
      ((N7, Down), N4);
      ((N8, Left), N7);
      ((N8, Right), N9);
      ((N8, Down), N5);
      ((N9, Left), N8);
      ((N9, Down), N6);
      ((N4, Up), N7);
      ((N4, Right), N5);
      ((N4, Down), N1);
      ((N5, Up), N8);
      ((N5, Left), N4);
      ((N5, Right), N6);
      ((N5, Down), N2);
      ((N6, Up), N9);
      ((N6, Left), N5);
      ((N6, Down), N3);
      ((N1, Up), N4);
      ((N1, Right), N2);
      ((N2, Up), N5);
      ((N2, Left), N1);
      ((N2, Right), N3);
      ((N2, Down), N0);
      ((N3, Up), N6);
      ((N3, Left), N2);
      ((N3, Down), NA);
      ((N0, Up), N2);
      ((N0, Right), NA);
      ((NA, Up), N3);
      ((NA, Left), N0);
    ]

module type Bfs = sig
  type element

  val adjacency : ((element * dirpad) * element) Seq.t
  val string_of_element : element -> string
end

module MakeBfs (B : Bfs) : sig
  include Bfs

  val shortest_paths : B.element -> B.element -> B.element list list
  val many_shortest_paths : B.element list -> dirpad list list
  val path_to_dirpad : B.element list -> dirpad list
end = struct
  type element = B.element

  let adjacency = B.adjacency
  let string_of_element = B.string_of_element

  module Vertex = struct
    type t = B.element

    let compare = compare
  end

  module Edge = struct
    type t = B.element * dirpad

    let compare = compare
  end

  module Graph = Map.Make (Edge)
  module VertexMap = Map.Make (Vertex)
  module VertexSet = Set.Make (Vertex)

  module DirGraph = Map.Make (struct
    type t = B.element * B.element

    let compare = compare
  end)

  let graph = adjacency |> Graph.of_seq

  let dir_graph =
    Graph.to_list graph
    |> List.map (fun ((el1, dir), el2) -> ((el1, el2), dir))
    |> DirGraph.of_list

  let dijkstra_memo = Hashtbl.create 10

  let dijkstra (src : B.element) =
    let dijkstra_unmemo src =
      let unvisited =
        Graph.to_seq graph |> Seq.map fst |> Seq.map fst |> VertexSet.of_seq
      in
      let distances =
        Graph.to_seq graph
        |> Seq.map (fun ((a, _), _) -> (a, max_int))
        |> VertexMap.of_seq
        |> VertexMap.update src (Option.map (fun _ -> 0))
      in
      let rec dijkstra_step distances unvisited parents =
        VertexSet.fold
          (fun node (mnode, mcost) ->
            let cost = VertexMap.find node distances in
            if cost < mcost then (Some node, cost) else (mnode, mcost))
          unvisited (None, max_int)
        |> function
        | None, _ -> (parents, distances)
        | Some node, cost ->
            let new_distances, new_parents =
              [ Up; Down; Left; Right ]
              |> List.filter_map (fun d -> Graph.find_opt (node, d) graph)
              |> List.filter (fun n -> VertexSet.mem n unvisited)
              |> List.fold_left
                   (fun (dist, par) new_node ->
                     let base_cost = VertexMap.find node dist in
                     let old_cost = VertexMap.find new_node dist in
                     let new_cost = base_cost + 1 in
                     if new_cost <= old_cost then
                       ( dist
                         |> VertexMap.update new_node (function _ ->
                                Some new_cost),
                         par
                         |> VertexMap.update new_node (function
                              | Some l -> Some (node :: l)
                              | None -> Some (node :: [])) )
                     else (dist, par))
                   (distances, parents)
            in
            let new_unvisited = VertexSet.remove node unvisited in
            dijkstra_step new_distances new_unvisited new_parents
      in

      dijkstra_step distances unvisited VertexMap.empty
    in
    Hashtbl.find_opt dijkstra_memo src |> function
    | Some r -> r
    | None ->
        let r = dijkstra_unmemo src in
        let _ = Hashtbl.replace dijkstra_memo src r in
        r

  let shortest_paths (src : B.element) (dst : B.element) =
    let parents, distances = dijkstra src in
    let rec traverse node visited acc_path paths =
      if List.mem node visited then []
      else if node = src then [ node :: acc_path ]
      else
        let children =
          VertexMap.find_opt node parents |> function Some c -> c | None -> []
        in
        List.concat
          (List.map
             (fun child ->
               traverse child (node :: visited) (node :: acc_path) paths)
             children)
    in

    traverse dst [] [] []

  let inspect_shortest_paths src dst =
    inspect (fun shortest_path ->
        let _ =
          printf "Path %s -> %s (%d): " (B.string_of_element src)
            (B.string_of_element dst)
            (List.length shortest_path)
        in
        let _ =
          List.iter
            (fun p -> printf "%s " (B.string_of_element p))
            shortest_path
        in
        let _ = printf "\n" in
        ())

  let path_to_dirpad path =
    path |> pairs |> List.map (fun (a, b) -> DirGraph.find (a, b) dir_graph)

  let many_shortest_paths points =
    let rec expand_paths (prev_paths : dirpad list list)
        (next_paths : 'a list list list) =
      match next_paths with
      | [] -> prev_paths
      | paths :: rest ->
          let current_paths =
            List.map
              (fun x -> List.map (fun y -> (DA :: x) @ y) prev_paths)
              paths
            |> List.flatten
          in
          expand_paths current_paths rest
    in

    let group_by_runs =
      List.fold_left
        (fun acc x ->
          match acc with
          | [] -> [ [ x ] ]
          | (h :: t) :: rest ->
              if h = x then (x :: h :: t) :: rest else [ x ] :: (h :: t) :: rest
          | _ -> raise Unreachable)
        []
    in

    let rec path_is_ordered path =
      let groups = group_by_runs (List.rev path) in
      List.length groups
      = List.length (List.sort_uniq compare (List.map List.hd groups))
    in

    points |> pairs
    |> List.map (fun (src, dst) -> shortest_paths src dst)
    |> List.map (fun x ->
           x |> List.map path_to_dirpad
           |> List.filter path_is_ordered
           |> List.map List.rev)
    |> expand_paths [ [] ] |> List.map List.rev
end

module NumpadBfs = MakeBfs (struct
  type element = numpad
  type edge = dirpad

  let adjacency = NumpadGraph.to_seq numpad_graph
  let string_of_element = string_of_numpad
end)

module DirpadBfs = MakeBfs (struct
  type element = dirpad
  type edge = dirpad

  let adjacency = DirpadGraph.to_seq dirpad_graph
  let string_of_element = string_of_dirpad
end)

let print_dir_path path =
  let _ = printf "Path has length %d: " (List.length path) in
  let _ = path |> List.iter (fun p -> printf "%s" (string_of_dirpad p)) in
  printf "%!\n"

let indirect_robot count code =
  let first_robot_paths = NumpadBfs.many_shortest_paths (NA :: code) in
  let _ = List.iter print_dir_path first_robot_paths in
  let rec indirect count paths =
    let min_length =
      paths |> List.map List.length |> List.fold_left min max_int
    in
    let paths = paths |> List.filter (fun p -> List.length p = min_length) in
    let _ =
      printf "Indirect %d with %d paths of length %d%!\n" count
        (List.length paths) min_length
    in
    let _ =
      List.iter (fun p -> printf "%s" (string_of_dirpad p)) (List.hd paths)
    in
    let _ = printf "\n" in
    if count = 0 then min_length
    else
      indirect (count - 1)
        (paths
        |> List.map (fun path -> DirpadBfs.many_shortest_paths (DA :: path))
        |> List.flatten)
  in
  indirect count first_robot_paths

let compute_complexity count codes =
  codes
  |> List.map (fun (code, num) -> num * indirect_robot count code)
  |> List.fold_left ( + ) 0

let part1 = compute_complexity 2
let part2 = compute_complexity 25
let _ = parse test_case |> part1 |> printf "Test 1: %d\n"
let _ = parse day_input |> part1 |> printf "Part 1: %d\n"
let _ = parse test_case |> part2 |> printf "Test 2: %d\n"
let _ = parse day_input |> part2 |> printf "Part 2: %d\n"
