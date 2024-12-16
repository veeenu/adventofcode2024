open Common
open Printf

let test_case1 =
  {|
###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############
|}
  |> String.trim |> String.split_on_char '\n'

let test_case2 =
  {|
#################
#...#...#...#..E#
#.#.#.#.#.#.#.#.#
#.#.#.#...#...#.#
#.#.#.#.###.#.#.#
#...#.#.#.....#.#
#.#.#.#.#.#####.#
#.#...#.#.#.....#
#.#.#####.#.###.#
#.#.#.......#...#
#.#.###.#####.###
#.#.#...#.....#.#
#.#.#.#####.###.#
#.#.#.........#.#
#.#.#.#########.#
#S#.............#
#################
|}
  |> String.trim |> String.split_on_char '\n'

let day_input = read_day_lines 16

type cell = Wall | Start | End | Space

module GraphTbl = Hashtbl.Make (struct
  type t = int * int * direction

  let equal i j = i = j

  let hash (i, j, d) =
    i + (1000 * j)
    + (d |> function
       | North -> 100000
       | South -> 200000
       | East -> 300000
       | West -> 400000)
end)

let tbl_push tbl k v =
  GraphTbl.replace tbl k
    (GraphTbl.find_opt tbl k |> function None -> v :: [] | Some l -> v :: l)

let turn_cost (d1 : direction) (d2 : direction) =
  match (d1, d2) with
  | North, North | South, South | West, West | East, East -> 1
  | North, East
  | North, West
  | South, East
  | South, West
  | East, North
  | East, South
  | West, North
  | West, South ->
      1001
  | North, South | South, North | West, East | East, West -> 2002

type day16_graph = (int * int * direction * int) list GraphTbl.t
(* (int * int * direction, (int * int * direction * int) list) Hashtbl.t *)

let parse input =
  let grid =
    input
    |> Grid.of_lines (function
         | '#' -> Wall
         | 'S' -> Start
         | 'E' -> End
         | '.' -> Space
         | _ -> raise Unreachable)
  in
  let graph : day16_graph =
    grid |> Grid.items
    |> List.filter (fun (_, _, t) -> t != Wall)
    |> List.fold_left
         (fun acc (x, y, _) ->
           let _ =
             [ North; South; West; East ]
             |> List.map (fun d -> (d, Direction.move d (x, y)))
             |> List.filter (fun (_, (nx, ny)) -> Grid.at grid nx ny != Wall)
             |> List.iter (fun (d, (nx, ny)) ->
                    [ North; South; West; East ]
                    |> List.iter (fun dc ->
                           tbl_push acc (x, y, dc) (nx, ny, d, turn_cost dc d)))
           in
           acc)
         (GraphTbl.create 100)
  in
  let start_x, start_y =
    grid |> Grid.items
    |> List.find_map (fun (x, y, t) -> if t = Start then Some (x, y) else None)
    |> Option.get
  in
  (* will have to choose the lowest among the (max) four (tx, ty, d) values at the end *)
  let target =
    grid |> Grid.items
    |> List.find_map (fun (x, y, t) -> if t = End then Some (x, y) else None)
    |> Option.get
  in
  (graph, (start_x, start_y), target)

let dijkstra (graph : day16_graph) start =
  let unvisited =
    GraphTbl.to_seq graph |> Seq.map fst
    |> Seq.fold_left
         (fun acc p ->
           let _ = GraphTbl.replace acc p () in
           acc)
         (GraphTbl.create (GraphTbl.length graph))
  in
  let distances =
    GraphTbl.to_seq graph |> Seq.map fst
    |> Seq.fold_left
         (fun acc p ->
           let _ = GraphTbl.replace acc p (if p = start then 0 else max_int) in
           acc)
         (GraphTbl.create (GraphTbl.length graph))
  in
  let preds =
    GraphTbl.to_seq graph |> Seq.map fst
    |> Seq.fold_left
         (fun acc p ->
           let _ = GraphTbl.replace acc p [] in
           acc)
         (GraphTbl.create (GraphTbl.length graph))
  in
  let update_distance node =
    let _ =
      GraphTbl.find graph node
      |> List.filter (fun (x, y, d, _) -> GraphTbl.mem unvisited (x, y, d))
      |> List.iter (fun (x, y, d, cost) ->
             let base_cost = GraphTbl.find distances node in
             let current_cost = GraphTbl.find distances (x, y, d) in
             let new_cost = cost + base_cost in
             if new_cost <= current_cost then
               let () = GraphTbl.replace distances (x, y, d) new_cost in
               let () = tbl_push preds (x, y, d) node in
               ())
    in
    let _ = GraphTbl.remove unvisited node in
    ()
  in
  let rec dijkstra_step () =
    GraphTbl.fold
      (fun node () (mnode, mcost) ->
        let cost = GraphTbl.find distances node in
        if cost < mcost then (Some node, cost) else (mnode, mcost))
      unvisited (None, max_int)
    |> function
    | None, _ -> ()
    | Some node, _ ->
        let _ = update_distance node in
        dijkstra_step ()
  in
  let _ = dijkstra_step () in
  (distances, preds)

let part1 input =
  let graph, (sx, sy), (tx, ty) = input |> parse in
  let distances, _ = dijkstra graph (sx, sy, East) in
  let candidates =
    [ North; South; West; East ]
    |> List.filter_map (fun d -> GraphTbl.find_opt distances (tx, ty, d))
  in
  List.fold_left (fun acc i -> min acc i) max_int candidates

let part2 input =
  let graph, (sx, sy), (tx, ty) = input |> parse in
  let distances, preds = dijkstra graph (sx, sy, East) in

  let rec walk node =
    node :: (GraphTbl.find preds node |> List.map walk |> List.flatten)
  in

  let candidates =
    [ North; South; West; East ]
    |> List.filter_map (fun d -> GraphTbl.find_opt distances (tx, ty, d))
  in
  let shortest_paths_length =
    List.fold_left (fun acc i -> min acc i) max_int candidates
  in
  [ North; South; West; East ]
  |> List.map (fun d -> (tx, ty, d))
  |> List.filter (fun p -> GraphTbl.find distances p = shortest_paths_length)
  |> List.map walk |> List.fold_left ( @ ) []
  |> List.map (fun (x, y, _) -> (x, y))
  |> List.sort_uniq compare |> List.length

let () = part1 test_case1 |> printf "Test 1: %d%!\n"
let () = part1 test_case2 |> printf "Test 1: %d%!\n"
let () = part1 day_input |> printf "Part 1: %d%!\n"
let () = part2 test_case1 |> printf "Test 2: %d%!\n"
let () = part2 test_case2 |> printf "Test 2: %d%!\n"
let () = part2 day_input |> printf "Part 2: %d%!\n"
