open Common
open Printf

let test_case =
  {|
MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX
|}
  |> String.trim |> String.split_on_char '\n' |> to_char_ll

let day_input = read_day_lines 4 |> to_char_ll

module Day04 (Input : sig
  val input : char list list
end) =
struct
  open Input

  let char_at x y = List.nth (List.nth input y) x
  let w = List.length (List.nth input 0)
  let h = List.length input
  let range_up start count = List.init count (fun i -> start + i)
  let range_dn start count = List.init count (fun i -> start - i)
  let dir_n y = if y >= 3 then Some (range_dn y 4) else None
  let dir_s y = if y <= h - 4 then Some (range_up y 4) else None
  let dir_w x = if x >= 3 then Some (range_dn x 4) else None
  let dir_e x = if x <= w - 4 then Some (range_up x 4) else None
  let case_horiz xs y = Option.map (fun xs -> List.combine xs [ y; y; y; y ]) xs
  let case_vert x ys = Option.map (fun ys -> List.combine [ x; x; x; x ] ys) ys

  let case_diag xs ys =
    match (xs, ys) with
    | Some xs, Some ys -> Some (List.combine xs ys)
    | _ -> None

  let case_n x y = case_vert x (dir_n y)
  let case_s x y = case_vert x (dir_s y)
  let case_e x y = case_horiz (dir_e x) y
  let case_w x y = case_horiz (dir_w x) y
  let case_ne x y = case_diag (dir_e x) (dir_n y)
  let case_nw x y = case_diag (dir_w x) (dir_n y)
  let case_se x y = case_diag (dir_e x) (dir_s y)
  let case_sw x y = case_diag (dir_w x) (dir_s y)

  let is_xmas xys =
    xys
    |> List.combine [ 'X'; 'M'; 'A'; 'S' ]
    |> List.for_all (fun (c, (x, y)) -> char_at x y == c)

  let indices =
    List.concat_map
      (fun y -> List.map (fun x -> (x, y)) (range_up 0 h))
      (range_up 0 w)

  let count_xmas_at (x, y) =
    List.fold_left ( + ) 0
      (List.filter_map
         (fun fn -> fn x y)
         [ case_n; case_s; case_e; case_w; case_ne; case_nw; case_se; case_sw ]
      |> List.map is_xmas
      |> List.map (function true -> 1 | false -> 0))

  let part1 =
    List.fold_left ( + ) 0
      (indices
      |> List.filter (fun (x, y) -> char_at x y = 'X')
      |> List.map count_xmas_at)
end

module TestCase = Day04 (struct
  let input = test_case
end)

module Input = Day04 (struct
  let input = day_input
end)

let () = assert (TestCase.part1 == 18)
let () = printf "Part 1: %d\n" Input.part1
