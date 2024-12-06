open Common
open Printf

let test_case =
  {|
....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...
|}
  |> String.trim |> String.split_on_char '\n'

exception InvalidInput

module Day06 (AocInput : AocInput) = struct
  open AocInput

  type direction = North | East | South | West
  type cell = Empty | Obstacle | Guard of direction

  let cell_of_char = function
    | '#' -> Obstacle
    | '.' -> Empty
    | 'v' -> Guard South
    | '^' -> Guard North
    | '>' -> Guard East
    | '<' -> Guard West
    | _ -> raise InvalidInput

  let char_of_dir = function
    | South -> 'v'
    | North -> '^'
    | East -> '>'
    | West -> '<'

  let turn = function
    | North -> East
    | East -> South
    | South -> West
    | West -> North

  let cells = Grid.of_lines cell_of_char input

  let start_x, start_y, start_dir =
    Grid.indices cells
    |> List.map (fun (x, y) -> (x, y, Grid.at cells x y))
    |> List.find_map (function x, y, Guard d -> Some (x, y, d) | _ -> None)
    |> function
    | Some x -> x
    | None -> raise InvalidInput

  let is_in_bounds x y =
    x >= 0 && y >= 0 && x < Grid.width cells && y < Grid.height cells

  type movement_outcome =
    | Moved of (int * int)
    | OutOfBounds
    | Obstacle of direction

  let original_path =
    let try_move x y dir =
      let nx, ny =
        match dir with
        | North -> (x, y - 1)
        | East -> (x + 1, y)
        | South -> (x, y + 1)
        | West -> (x - 1, y)
      in
      if not (is_in_bounds nx ny) then OutOfBounds
      else if Grid.at cells nx ny = Obstacle then Obstacle (turn dir)
      else Moved (nx, ny)
    in

    let rec step visited x y dir =
      let () = Hashtbl.replace visited (x, y) () in
      try_move x y dir |> function
      | OutOfBounds -> visited
      | Obstacle ndir -> step visited x y ndir
      | Moved (nx, ny) -> step visited nx ny dir
    in
    step (Hashtbl.create 0) start_x start_y start_dir

  let part1 () = original_path |> Hashtbl.length

  let try_with_obstacle ox oy =
    let try_move x y dir =
      let nx, ny =
        match dir with
        | North -> (x, y - 1)
        | East -> (x + 1, y)
        | South -> (x, y + 1)
        | West -> (x - 1, y)
      in
      if not (is_in_bounds nx ny) then OutOfBounds
      else if Grid.at cells nx ny = Obstacle || (nx = ox && ny == oy) then
        Obstacle (turn dir)
      else Moved (nx, ny)
    in

    (*
        maximum amount of steps before finding a collision is width * height,
        so this is guaranteed to return true or false eventually
    *)
    let rec step visited x y dir =
      let is_visited = Hashtbl.find_opt visited (x, y, dir) |> Option.is_some in
      let () = Hashtbl.replace visited (x, y, dir) () in
      if is_visited then true
      else
        try_move x y dir |> function
        | OutOfBounds -> false
        | Obstacle ndir -> (step [@tailcall]) visited x y ndir
        | Moved (nx, ny) -> (step [@tailcall]) visited nx ny dir
    in
    step (Hashtbl.create 0) start_x start_y start_dir

  let part2 () =
    Hashtbl.to_seq original_path
    |> List.of_seq |> List.map fst
    |> List.filter (fun (x, y) ->
           Grid.at cells x y != Obstacle && not (x == start_x && y == start_y))
    |> List.map (fun (x, y) -> try_with_obstacle x y)
    |> List.fold_left (fun acc x -> acc + if x then 1 else 0) 0
end

module TestCase = Day06 (struct
  let input = test_case
end)

module DayInput = Day06 (struct
  let input = read_day_lines 6
end)

let () = assert (TestCase.part1 () = 41)
let () = printf "Part 1: %d\n" (DayInput.part1 ())
let () = assert (TestCase.part2 () = 6)
let () = printf "Part 2: %d\n" (DayInput.part2 ())
