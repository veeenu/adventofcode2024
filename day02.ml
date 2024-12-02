open Common

let test_case =
  {|
7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9
  |}
  |> split_lines

let parse line = String.split_on_char ' ' line |> List.map int_of_string

let rec pairs list =
  match list with x :: (y :: _ as xs) -> (x, y) :: pairs xs | _ -> []

let diffs = List.map (fun (x, y) -> x - y)

let is_limited diffs =
  diffs |> List.map abs |> List.for_all (fun x -> x >= 1 && x <= 3)

let is_increasing = List.for_all (fun x -> x >= 0)
let is_decreasing = List.for_all (fun x -> x <= 0)
let is_safe list = is_limited list && (is_increasing list || is_decreasing list)
let is_safe_line line = line |> parse |> pairs |> diffs |> is_safe

(* Part 1 *)

let part1 input =
  input |> List.map is_safe_line
  |> List.fold_left (fun acc safe -> acc + if safe then 1 else 0) 0

let () = assert (part1 test_case = 2)
let () = read_day_lines 2 |> part1 |> Printf.printf "Part 1: %d\n" 