open Common
open Printf
open Scanf

let test_case =
  {|
5,4
4,2
4,5
3,0
2,1
6,3
2,4
1,5
0,6
3,3
2,6
5,1
1,2
5,5
2,5
6,5
1,4
0,4
6,4
1,1
6,1
1,0
0,5
1,6
2,0
|}
  |> String.trim |> String.split_on_char '\n'

let day_input = read_day_lines 18

let parse input =
  input |> List.map (fun s -> sscanf s "%d,%d" (fun x y -> (x, y)))

type cell = Normal | Corrupted

let char_of_cell = function Normal -> '.' | Corrupted -> '#'
let make_grid w h = Array.make_matrix w h Normal

let print_grid grid =
  Array.iter
    (fun row ->
      let _ = Array.iter (fun col -> printf "%c" (char_of_cell col)) row in
      printf "%!\n")
    grid

let rec simulate grid points count =
  if count = 0 then grid
  else
    let x, y = List.hd points in
    let _ = grid.(y).(x) <- Corrupted in
    simulate grid (List.tl points) (count - 1)

let pathfind grid (tx, ty) =
  let visited =
    Array.make_matrix (Array.length grid) (Array.length grid.(0)) false
  in
  let edges =
    Array.make_matrix (Array.length grid) (Array.length grid.(0)) (-1, -1)
  in
  let q = Queue.create () in
  let _ = Queue.add (0, 0) q in
  let rec step xs =
    if Queue.is_empty q then false
    else
      let x, y = Queue.take q in
      if (x, y) = (tx, ty) then true
      else
        let neighbors =
          [ South; East; West; North ]
          |> List.map (fun d -> Direction.move d (x, y))
          |> List.filter (fun (x, y) -> x >= 0 && y >= 0 && x <= tx && y <= ty)
          |> List.filter (fun (x, y) -> not visited.(y).(x))
          |> List.filter (fun (x, y) -> grid.(y).(x) = Normal)
        in
        let _ =
          neighbors
          |> List.iter (fun (nx, ny) ->
                 let _ = visited.(ny).(nx) <- true in
                 let _ = edges.(ny).(nx) <- (x, y) in
                 let _ = Queue.add (nx, ny) q in
                 ())
        in
        step ((x, y) :: xs)
  in
  let rec traverse (x, y) =
    let p = edges.(y).(x) in
    if p = (0, 0) then [ p ] else p :: traverse p
  in
  if step [] then Some (traverse (tx, ty)) else None

let part1 input w h steps =
  let grid = simulate (make_grid (w + 1) (h + 1)) input steps in
  let () = print_grid grid in
  let path = pathfind grid (w, h) |> Option.get in
  List.length path

let part2 input w h =
  let rec test_sim steps =
    let grid = simulate (make_grid (w + 1) (h + 1)) input steps in
    let path = pathfind grid (w, h) |> Option.is_some in
    if path then test_sim (steps + 1) else steps + 1
  in
  let steps = test_sim 0 in
  let grid_before = simulate (make_grid (w + 1) (h + 1)) input (steps - 2) in
  let grid_after = simulate (make_grid (w + 1) (h + 1)) input (steps - 1) in
  let grid_cmp =
    Array.map2
      (fun row1 row2 -> Array.map2 (fun col1 col2 -> col1 = col2) row1 row2)
      grid_before grid_after
  in
  let x, y =
    Array.find_mapi
      (fun y row ->
        Array.find_mapi (fun x col -> if not col then Some (x, y) else None) row)
      grid_cmp
    |> Option.get
  in
  sprintf "%d,%d" x y

let _ = printf "Test 1: %d%!\n" (part1 (parse test_case) 6 6 12)
let _ = printf "Part 1: %d%!\n" (part1 (parse day_input) 70 70 1024)
let _ = printf "Test 2: %s%!\n" (part2 (parse test_case) 6 6)
let _ = printf "Part 2: %s%!\n" (part2 (parse day_input) 70 70)
