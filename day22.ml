open Common
open Printf

let test_case = {|
1
10
100
2024
|} |> String.trim |> String.split_on_char '\n'

let day_input = read_day_lines 22
let parse = List.map int_of_string

let evolve secret =
  let step1 = Int.shift_left secret 6 lxor secret land 16777215 in
  let step2 = Int.shift_right step1 5 lxor step1 land 16777215 in
  let step3 = Int.shift_left step2 11 lxor step2 land 16777215 in
  step3

let rec evolve_times count secret =
  if count = 0 then secret else evolve_times (count - 1) (evolve secret)

let part1 input =
  parse input |> List.map (evolve_times 2000) |> List.fold_left ( + ) 0

let _ = part1 test_case |> printf "Test 1: %d\n"
let _ = part1 day_input |> printf "Part 1: %d\n"
