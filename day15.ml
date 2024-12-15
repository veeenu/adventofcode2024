open Common
open Printf

let test_case1 =
  {|
########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<
|}
  |> String.trim |> String.split_on_char '\n'

let test_case2 =
  {|
##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^
|}
  |> String.trim |> String.split_on_char '\n'

let day_input = read_day_lines 15

type field_items = Empty | Crate | Wall

type input_state = {
  grid : field_items array array;
  movements : direction list;
  position : int * int;
}

let parse input =
  let non_empty s = not (String.equal (String.trim s) "") in
  let grid_lines = input |> take_while non_empty in
  let position =
    input |> List.find_index (fun s -> String.contains s '@') |> Option.get
    |> fun y ->
    List.nth grid_lines y |> fun s -> (String.index s '@', y)
  in
  let grid =
    grid_lines
    |> List.map (fun line ->
           line |> String.trim |> String.to_seq
           |> Seq.map (function
                | '#' -> Wall
                | 'O' -> Crate
                | '.' | '@' -> Empty
                | _ -> raise Unreachable)
           |> Array.of_seq)
    |> Array.of_list
  in
  let movements =
    input |> drop_while non_empty |> List.map String.trim |> String.concat ""
    |> String.to_seq
    |> Seq.map (function
         | '^' -> North
         | '>' -> East
         | 'v' -> South
         | '<' -> West
         | _ -> raise Unreachable)
    |> List.of_seq
  in
  { position; grid; movements }

let print_grid grid position =
  let () =
    Array.iteri
      (fun y row ->
        let () =
          row
          |> Array.mapi (fun x cell ->
                 match cell with
                 | _ when position = (x, y) -> '@'
                 | Wall -> '#'
                 | Empty -> '.'
                 | Crate -> 'O')
          |> Array.iter (printf "%c")
        in
        printf "\n")
      grid
  in
  printf "\n"

let rec apply_movement state =
  let rec pull grid (x, y) dir count =
    let nx, ny = Direction.move dir (x, y) in
    let t = grid.(y).(x) in
    let () = grid.(y).(x) <- grid.(ny).(nx) in
    let () = grid.(ny).(nx) <- t in
    if count = 0 then true else pull grid (nx, ny) dir (count - 1)
  in

  let rec try_push grid position dir count =
    let nx, ny = Direction.move dir position in
    match grid.(ny).(nx) with
    | Wall -> false
    | Empty -> pull grid (nx, ny) (Direction.flip dir) count
    | Crate -> try_push grid (nx, ny) dir (count + 1)
  in

  let move grid position dir =
    let nx, ny = Direction.move dir position in
    match grid.(ny).(nx) with
    | Wall -> (grid, position)
    | Empty -> (grid, (nx, ny))
    | Crate ->
        if try_push grid (nx, ny) dir 0 then (grid, (nx, ny))
        else (grid, position)
  in

  match state.movements with
  | [] -> state
  | hd :: movements ->
      let grid, position = move state.grid state.position hd in
      apply_movement { grid; movements; position }

let compute_gps state =
  state.grid
  |> Array.mapi (fun y row ->
         Array.mapi
           (fun x cell -> if cell == Crate then Some (x, y) else None)
           row
         |> Array.to_list
         |> List.filter_map (fun x -> x))
  |> Array.to_list |> List.flatten
  |> List.map (fun (x, y) -> x + (100 * y))
  |> List.fold_left ( + ) 0

let part1 state = apply_movement state |> compute_gps
let () = test_case1 |> parse |> part1 |> printf "Test 1: %d\n"
let () = test_case2 |> parse |> part1 |> printf "Test 1: %d\n"
let () = day_input |> parse |> part1 |> printf "Part 1: %d\n"

(* let state = test_case2 |> parse in *)
(* let () = print_grid state.grid state.position in *)
(* let state = apply_movement state in *)
(* let () = print_grid state.grid state.position in *)
(* let () = printf "%d\n" (compute_gps state) in *)
(**)
(* () *)
