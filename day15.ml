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

let test_case3 =
  {|
#######
#...#.#
#.....#
#..OO@#
#..O..#
#.....#
#######

<vv<<^^<<^^
|}
  |> String.trim |> String.split_on_char '\n'

let day_input = read_day_lines 15

type field_items = Empty | Crate | Wall
type field_items2 = Empty | CrateL | CrateR | Wall

type 'a input_state = {
  grid : 'a array array;
  movements : direction list;
  position : int * int;
}

let field_map1 : char -> field_items list = function
  | '#' -> [ Wall ]
  | 'O' -> [ Crate ]
  | '.' | '@' -> [ Empty ]
  | _ -> raise Unreachable

let field_map2 : char -> field_items2 list = function
  | '#' -> [ Wall; Wall ]
  | 'O' -> [ CrateL; CrateR ]
  | '.' | '@' -> [ Empty; Empty ]
  | _ -> raise Unreachable

let parse field_map input =
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
           line |> String.trim |> String.to_seq |> List.of_seq
           |> List.map field_map |> List.flatten |> Array.of_list)
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

let parse1 = parse field_map1

let parse2 input =
  let { position = x, y; grid; movements } = parse field_map2 input in
  let position = (x * 2, y) in
  { position; grid; movements }

let swap grid (x, y) (nx, ny) =
  let t = grid.(y).(x) in
  let () = grid.(y).(x) <- grid.(ny).(nx) in
  let () = grid.(ny).(nx) <- t in
  ()

let rec pull1 grid (x, y) dir count =
  let nx, ny = Direction.move dir (x, y) in
  let () = swap grid (x, y) (nx, ny) in
  if count = 0 then true else pull1 grid (nx, ny) dir (count - 1)

let rec try_push1 (grid : field_items array array) position dir count =
  let nx, ny = Direction.move dir position in
  match grid.(ny).(nx) with
  | Wall -> false
  | Empty -> pull1 grid (nx, ny) (Direction.flip dir) count
  | Crate -> try_push1 grid (nx, ny) dir (count + 1)

let move1 (grid : field_items array array) position dir =
  let nx, ny = Direction.move dir position in
  match grid.(ny).(nx) with
  | Wall -> (grid, position)
  | Empty -> (grid, (nx, ny))
  | Crate ->
      if try_push1 grid (nx, ny) dir 0 then (grid, (nx, ny))
      else (grid, position)

type push_action =
  | Clear of (int * int)
  | SetL of (int * int)
  | SetR of (int * int)

let combine l r = match (l, r) with Some l, Some r -> Some (l @ r) | _ -> None

let rec push_horiz2 (grid : field_items2 array array) position dir =
  let () = assert (dir = East || dir = West) in
  let nx, ny = Direction.move dir position in
  match grid.(ny).(nx) with
  | Wall -> None
  | Empty -> Some []
  | CrateL ->
      combine
        (push_horiz2 grid (nx, ny) dir)
        (Some
           [ Clear position; SetR (nx, ny); SetL (Direction.move dir (nx, ny)) ])
  | CrateR ->
      combine
        (push_horiz2 grid (nx, ny) dir)
        (Some
           [ Clear position; SetL (nx, ny); SetR (Direction.move dir (nx, ny)) ])

let rec push_vert2 (grid : field_items2 array array) position dir =
  let () = assert (dir = North || dir = South) in

  let push_crate position side =
    let nx, ny = Direction.move dir position in
    let moves =
      match side with
      | CrateL ->
          [
            Clear position;
            Clear (Direction.move East position);
            SetL (nx, ny);
            SetR (Direction.move East (nx, ny));
          ]
      | CrateR ->
          [
            Clear position;
            Clear (Direction.move West position);
            SetR (nx, ny);
            SetL (Direction.move West (nx, ny));
          ]
      | _ -> raise Unreachable
    in
    match grid.(ny).(nx) with
    | Wall -> None
    | Empty -> Some moves
    | CrateL -> combine (push_vert2 grid (nx, ny) dir) (Some moves)
    | CrateR -> combine (push_vert2 grid (nx, ny) dir) (Some moves)
  in

  let x, y = position in
  match grid.(y).(x) with
  | Wall -> None
  | Empty -> Some []
  | CrateL ->
      combine
        (push_crate position CrateL)
        (push_crate (Direction.move East position) CrateR)
  | CrateR ->
      combine
        (push_crate position CrateR)
        (push_crate (Direction.move West position) CrateL)

let push2 (grid : field_items2 array array) position dir =
  match dir with
  | East | West -> push_horiz2 grid position dir
  | North | South -> push_vert2 grid position dir

let apply (grid : field_items2 array array) changes =
  let () =
    changes
    |> List.filter_map (function Clear (x, y) -> Some (x, y) | _ -> None)
    |> List.iter (fun (x, y) -> grid.(y).(x) <- Empty)
  in
  let () =
    changes
    |> List.filter_map (function SetL (x, y) -> Some (x, y) | _ -> None)
    |> List.iter (fun (x, y) -> grid.(y).(x) <- CrateL)
  in
  let () =
    changes
    |> List.filter_map (function SetR (x, y) -> Some (x, y) | _ -> None)
    |> List.iter (fun (x, y) -> grid.(y).(x) <- CrateR)
  in
  ()

let try_push2 (grid : field_items2 array array) position dir =
  let nx, ny = Direction.move dir position in
  match push2 grid (nx, ny) dir with
  | Some swaps ->
      (* let _ = swaps |> List.rev |> List.iter (fun (a, b) -> swap grid a b) in *)
      let _ = swaps |> apply grid in
      (grid, (nx, ny))
  | None -> (grid, position)

let move2 (grid : field_items2 array array) position dir =
  let nx, ny = Direction.move dir position in
  (* let _ = print_grid grid position in *)
  let grid, position =
    match grid.(ny).(nx) with
    | Wall -> (grid, position)
    | Empty -> (grid, (nx, ny))
    | CrateR | CrateL -> try_push2 grid position dir
  in
  (grid, position)

let rec apply_movement move state =
  match state.movements with
  | [] -> state
  | hd :: movements ->
      let grid, position = move state.grid state.position hd in
      apply_movement move { grid; movements; position }

let compute_gps target state =
  state.grid
  |> Array.mapi (fun y row ->
         Array.mapi
           (fun x cell -> if cell == target then Some (x, y) else None)
           row
         |> Array.to_list
         |> List.filter_map (fun x -> x))
  |> Array.to_list |> List.flatten
  |> List.map (fun (x, y) -> x + (100 * y))
  |> List.fold_left ( + ) 0

let part1 input = input |> parse1 |> apply_movement move1 |> compute_gps Crate
let part2 input = input |> parse2 |> apply_movement move2 |> compute_gps CrateL
let () = test_case1 |> part1 |> printf "Test 1: %d\n"
let () = test_case2 |> part1 |> printf "Test 1: %d\n"
let () = day_input |> part1 |> printf "Part 1: %d\n"
let () = test_case1 |> part2 |> printf "Test 2: %d\n"
let () = test_case2 |> part2 |> printf "Test 2: %d\n"
let () = test_case3 |> part2 |> printf "Test 2: %d\n"
let () = day_input |> part2 |> printf "Part 2: %d\n"
