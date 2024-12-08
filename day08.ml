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

  let char_of_freq = function
    | i when i >= 0 && i <= 9 -> Char.chr (i + Char.code '0')
    | i when i >= 10 && i < 36 -> Char.chr (i - 10 + Char.code 'a')
    | i when i >= 36 && i < 62 -> Char.chr (i - 36 + Char.code 'A')
    | _ -> raise (Invalid "char_of_freq")

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
    l
    |> List.map candidate_antinodes
    |> List.flatten |> List.filter valid_antinode

  let part1 =
    freq_positions |> Array.map pairs |> Array.to_list |> List.map antinodes
    |> List.flatten
    |> List.sort_uniq (fun (x1, y1) (x2, y2) -> compare (y1, x1) (y2, x2))
    |> List.length
end

module TestCase = Day06 (struct
  let input = test_case
end)

module DayInput = Day06 (struct
  let input = read_day_lines 8
end)

let () = printf "Part 1 test case: %d\n" TestCase.part1
let () = printf "Part 1: %d\n" DayInput.part1
