open Common
open Printf

let test_case =
  {|
###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############
|}
  |> String.trim |> String.split_on_char '\n'

let day_input = read_day_lines 20

type cell = Wall | Start | End | Space

module Point = struct
  type t = int * int

  let compare = compare
end

module PointMap = Map.Make (Point)
module PointSet = Set.Make (Point)

let parse =
  ArrayGrid.of_lines (function
    | '#' -> Wall
    | '.' -> Space
    | 'S' -> Start
    | 'E' -> End
    | _ -> raise Unreachable)

let string_of_cell = function
  | Wall -> "#"
  | Space -> "."
  | Start -> "S"
  | End -> "E"

let to_graph grid =
  let make_neighbors x y =
    ( (x, y),
      ArrayGrid.neighbors grid x y
      |> List.filter (fun (x, y) -> ArrayGrid.at grid x y <> Wall) )
  in
  ArrayGrid.items grid
  |> List.filter_map (fun (x, y, i) ->
         match i with
         | Wall -> None
         | Space | Start | End -> Some (make_neighbors x y))
  |> List.fold_left
       (fun acc (k, v) ->
         PointMap.update k
           (function Some l -> Some (v @ l) | None -> Some v)
           acc)
       PointMap.empty

let find_path graph (sx, sy) (ex, ey) =
  let rec bfs graph visited parents = function
    | [] -> None
    | (x, y) :: q when PointSet.mem (x, y) visited ->
        bfs graph visited parents q
    | (x, y) :: q ->
        if (x, y) = (ex, ey) then Some parents
        else
          let neighbors =
            PointMap.find_opt (x, y) graph
            |> Option.value ~default:[]
            |> List.filter (fun p -> PointSet.mem p visited |> not)
          in
          let new_queue = q @ neighbors in
          let new_parents =
            neighbors
            |> List.fold_left (fun acc n -> PointMap.add n (x, y) acc) parents
          in
          let new_visited = PointSet.add (x, y) visited in
          bfs graph new_visited new_parents new_queue
  in

  let rec traverse p q parents : (int * int) list =
    if p = q then []
    else
      let next = PointMap.find p parents in
      next :: traverse next q parents
  in

  let path = bfs graph PointSet.empty PointMap.empty [ (sx, sy) ] in

  let path =
    path
    |> Option.map (traverse (ex, ey) (sx, sy))
    |> Option.map (fun p -> (ex, ey) :: p)
    |> Option.map List.rev
  in

  path

let print_path grid visitations =
  ArrayGrid.print grid (fun x y col ->
      if List.mem (x, y) visitations then "\x1b[31m*\x1b[0m"
      else string_of_cell col)

let print_point grid p =
  ArrayGrid.print grid (fun x y col ->
      if p = (x, y) then "\x1b[31m*\x1b[0m" else string_of_cell col)

let solve input range bound =
  let grid = parse input in
  let sp =
    ArrayGrid.items grid
    |> List.find_map (fun (x, y, v) -> if v == Start then Some (x, y) else None)
    |> Option.get
  in

  let ep =
    ArrayGrid.items grid
    |> List.find_map (fun (x, y, v) -> if v == End then Some (x, y) else None)
    |> Option.get
  in

  let graph = to_graph grid in

  let glitchless_path = find_path graph sp ep |> Option.get in
  let _ = printf "Default path is %d long%!\n" (List.length glitchless_path) in

  let neighborhood_points =
    range_up (-range) ((range * 2) + 1)
    |> List.map (fun x ->
           range_up (-range) ((range * 2) + 1) |> List.map (fun y -> (x, y)))
    |> List.flatten
    |> List.filter (fun (x, y) ->
           (x, y) <> (0, 0) && abs x + abs y <= range && abs x + abs y > 1)
  in

  let cheat_edges (x, y) =
    neighborhood_points
    |> List.map (fun (dx, dy) -> (x + dx, y + dy))
    |> List.filter (fun (x1, y1) ->
           ArrayGrid.is_in_bounds grid x1 y1 && ArrayGrid.at grid x1 y1 != Wall)
    |> List.filter_map (fun (x1, y1) ->
           if (x, y) <> (x1, y1) then Some ((x, y), (x1, y1)) else None)
    |> List.sort_uniq compare
  in

  let cheat_edges = glitchless_path |> List.map cheat_edges |> List.flatten in

  let check_cheat (p1, p2) =
    let index1 = List.find_index (( = ) p1) glitchless_path in
    let index2 = List.find_index (( = ) p2) glitchless_path in
    let distance = abs (fst p1 - fst p2) + abs (snd p1 - snd p2) in
    match (index1, index2) with
    | Some i1, Some i2 ->
        i2 - i1 - distance |> fun i -> if i > 0 then Some i else None
    | _ -> None
  in

  let _ = printf "There are %d possible cheats%!\n" (List.length cheat_edges) in

  let cheats =
    cheat_edges |> List.filter_map check_cheat |> List.filter (( <= ) bound)
  in

  cheats |> List.length

let part1 input = solve input 2 
let part2 input = solve input 20

let () = part1 test_case 0 |> printf "Test 1: %d%!\n"
let () = part1 day_input 100 |> printf "Part 1: %d%!\n"
let () = part2 test_case 50 |> printf "Test 2: %d%!\n"
let () = part2 day_input 100 |> printf "Part 2: %d%!\n"
