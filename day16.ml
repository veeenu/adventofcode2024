open Common
open Printf

let test_case =
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

let day_input = read_day_lines 16

type cell = Wall | Start | End | Space

let tbl_push tbl k v =
  Hashtbl.replace tbl k
    (Hashtbl.find_opt tbl k |> function None -> v :: [] | Some l -> v :: l)

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

type day16_graph =
  (int * int * direction, (int * int * direction * int) list) Hashtbl.t

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
         (Hashtbl.create 100)
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
    Hashtbl.to_seq graph |> Seq.map fst
    |> Seq.fold_left
         (fun acc p ->
           let _ = Hashtbl.replace acc p () in
           acc)
         (Hashtbl.create (Hashtbl.length graph))
  in
  let distances =
    Hashtbl.to_seq graph |> Seq.map fst
    |> Seq.fold_left
         (fun acc p ->
           let _ = Hashtbl.replace acc p (if p = start then 0 else max_int) in
           acc)
         (Hashtbl.create (Hashtbl.length graph))
  in
  let update_distance node =
    let _ =
      Hashtbl.find graph node
      |> List.iter (fun (x, y, d, cost) ->
             let base_cost = Hashtbl.find distances node in
             let current_cost = Hashtbl.find distances (x, y, d) in
             let new_cost = cost + base_cost in
             if new_cost < current_cost then
               Hashtbl.replace distances (x, y, d) new_cost)
    in
    let _ = Hashtbl.remove unvisited node in
    ()
  in
  let rec dijkstra_step () =
    let _ = printf "Left %d%!\n" (Hashtbl.length unvisited) in
    let candidates =
      Hashtbl.to_seq unvisited
      |> Seq.map (fun (node, _) -> (node, Hashtbl.find distances node))
      |> Seq.filter (fun (_, cost) -> cost != max_int)
      |> List.of_seq
      |> List.sort (fun (_, acost) (_, bcost) -> compare acost bcost)
    in
    if List.length candidates = 0 then ()
    else
      let node, _ = List.hd candidates in
      let _ = update_distance node in
      dijkstra_step ()
  in
  let _ = dijkstra_step () in
  distances

let part1 input =
  let graph, (sx, sy), (tx, ty) = input |> parse in
  let distances = dijkstra graph (sx, sy, East) in
  let candidates =
    [ North; South; West; East ]
    |> List.filter_map (fun d -> Hashtbl.find_opt distances (tx, ty, d))
  in
  List.fold_left (fun acc i -> min acc i) max_int candidates

let () = part1 test_case |> printf "Test 1: %d\n"
let () = part1 day_input |> printf "Part 1: %d\n"
