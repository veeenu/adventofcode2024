open Common
open Printf

let test_case =
  {|
r, wr, b, g, bwu, rb, gb, br

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb
|}
  |> String.trim |> String.split_on_char '\n'

let day_input = read_day_lines 19
let cltos = List.to_seq >> String.of_seq
let stocl = String.to_seq >> List.of_seq

let parse input =
  let towels =
    List.hd input |> String.split_on_char ','
    |> List.map (String.trim >> String.to_seq >> List.of_seq)
  in
  let designs =
    input |> List.tl |> List.tl |> List.map (String.to_seq >> List.of_seq)
  in
  (towels, designs)

let rec is_prefix design_block towel =
  match (design_block, towel) with
  | ds, [] -> Some ds
  | [], _ -> None
  | d :: ds, t :: ts -> if d = t then is_prefix ds ts else None

let find_a_combination (towels : char list list) (design : char list) =
  let neighbors design = towels |> List.filter_map (is_prefix design) in

  let rec search q =
    match q with
    | [] -> false
    | next_block :: rest ->
        if List.is_empty next_block then true
        else neighbors next_block |> List.rev_append rest |> search
  in

  search [ design ]

let count_combinations (towels : char list list) (design : char list) =
  let neighbors design = towels |> List.filter_map (is_prefix design) in
  let search_memo = Memo.empty () in

  let rec search = function
    | [] -> 1
    | rest_design ->
        Memo.memo search_memo rest_design (fun rest_design ->
            neighbors rest_design |> List.map search |> List.fold_left ( + ) 0)
  in

  search design

let print_combo (combo : char list list) =
  let _ = printf "Combo of length %d: " (List.length combo) in
  let _ = List.iter (fun block -> printf "%s, " (cltos block)) combo in
  printf "\n"

let part1 input =
  let towels, designs = parse input in
  designs |> List.filter (find_a_combination towels) |> List.length

let part2 input =
  let towels, designs = parse input in
  designs |> List.map (count_combinations towels) |> List.fold_left ( + ) 0

let _ = part1 test_case |> printf "Test 1: %d\n"
let _ = part1 day_input |> printf "Part 1: %d\n"
let _ = part2 test_case |> printf "Test 2: %d\n"
let _ = part2 day_input |> printf "Part 2: %d\n"
