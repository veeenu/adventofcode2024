open Common
open Printf

let test_case =
  {|
............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............
|}
  |> String.trim |> String.split_on_char '\n'

(*
......#....#
...#....0...
....#0....#.
..#....0....
....0....#..
.#....A.....
...#........
#......#....
........A...
.........A..
..........#.
..........#.

(6, 0) (11, 0) 
(3, 1) 
(4, 2) (10, 2) 
(2, 3)
(9, 4)
(1, 5) (6, 5)
(3, 6)
(0, 7)
(7, 7)
(10, 10)
(10, 11)
 *)
exception Invalid of string
exception Unreachable of string

module Day06 (AocInput : AocInput) = struct
  open AocInput

  type freq = Freq of int | Empty

  let freq_of_char = function
    | c when c >= '0' && c <= '9' -> Freq (Char.code c - Char.code '0')
    | c when c >= 'a' && c <= 'z' -> Freq (Char.code c - Char.code 'a' + 10)
    | c when c >= 'A' && c <= 'Z' -> Freq (Char.code c - Char.code 'A' + 10 + 26)
    | c when c == '.' -> Empty
    | c -> raise (Invalid (sprintf "freq_of_char %c" c))

  let grid = Grid.of_lines freq_of_char input
  let freq_positions = Array.make 62 []

  let () =
    Grid.items grid
    |> List.filter (fun (_, _, t) -> t != Empty)
    |> List.iter (fun (x, y, t) ->
           match t with
           | Freq c -> freq_positions.(c) <- (x, y) :: freq_positions.(c)
           | Empty -> raise (Unreachable "freq_positions"))

  let rec pairs towers =
    match towers with
    | [] -> []
    | _ :: [] -> []
    | [ a; b ] -> [ (a, b) ]
    | hd :: tl -> List.map (fun el -> (hd, el)) tl @ pairs tl

  let candidate_antinodes ((x1, y1), (x2, y2)) =
    [ (x2 + x2 - x1, y2 + y2 - y1); (x1 + x1 - x2, y1 + y1 - y2) ]

  let valid_antinode (x, y) = Grid.is_in_bounds grid x y

  let antinodes l =
    List.map candidate_antinodes l |> List.flatten |> List.filter valid_antinode

  let freq_pairs = freq_positions |> Array.map pairs |> Array.to_list

  let part1 =
    freq_pairs |> List.map antinodes |> List.flatten
    |> List.sort_uniq (fun (x1, y1) (x2, y2) -> compare (y1, x1) (y2, x2))
    |> List.length

  let candidate_harmonic_antinodes pair =
    let p1, p2 = pair in
    let distances ((x1, y1), (x2, y2)) =
      ((x1 - x2, y1 - y2), (x2 - x1, y2 - y1))
    in
    let d1, d2 = distances pair in
    let rec candidate_harmonic_antinode (x, y) (dx, dy) =
      let cx, cy = (x + dx, y + dy) in
      if Grid.is_in_bounds grid cx cy then
        (cx, cy) :: candidate_harmonic_antinode (cx, cy) (dx, dy)
      else []
    in
    candidate_harmonic_antinode p1 d1 @ candidate_harmonic_antinode p2 d2

  let harmonic_antinodes l =
    List.map candidate_harmonic_antinodes l |> List.flatten

  let part2 =
    let antennae = Array.to_list freq_positions |> List.flatten in
    let harmonic_antinodes =
      List.map harmonic_antinodes freq_pairs |> List.flatten
    in

    antennae @ harmonic_antinodes
    |> List.sort_uniq (fun (x1, y1) (x2, y2) -> compare (y1, x1) (y2, x2))
    |> List.length
end

module TestCase = Day06 (struct
  let input = test_case
end)

module DayInput = Day06 (struct
  let input = read_day_lines 8
end)

let () = printf "Test 1: %d\n" TestCase.part1
let () = printf "Part 1: %d\n" DayInput.part1
let () = printf "Test 2: %d\n" TestCase.part2
let () = printf "Part 2: %d\n" DayInput.part2
