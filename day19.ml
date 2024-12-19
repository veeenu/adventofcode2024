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

let rec is_prefix design_block towel = match (design_block, towel) with
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

(* let count_combinations = Memo.make (fun design ->  *)
(**)
(* ) *)

(* let find_combinations (towels : char list list) (design : char list) = *)
(*   let find_design_prefixes design = *)
(*     Memo.memo prefixes_memo design (fun design -> *)
(*         towels *)
(*         |> List.filter_map (fun towel -> *)
(*                is_prefix design towel *)
(*                |> Option.map (fun block -> (towel, block)))) *)
(*   in *)
(**)
(*   let rec find_combinations_inner design : char list list list = *)
(*     if List.is_empty design then [ [] ] *)
(*     else *)
(*       find_design_prefixes design *)
(*       |> List.map (fun (towel, block) -> *)
(*              find_combinations_inner block *)
(*              |> List.map (fun next_combinations -> towel :: next_combinations)) *)
(*       |> List.flatten *)
(*   in *)
(**)
(*   find_combinations_inner design |> List.filter (List.is_empty >> not) *)

let print_combo (combo : char list list) =
  let _ = printf "Combo of length %d: " (List.length combo) in
  let _ = List.iter (fun block -> printf "%s, " (cltos block)) combo in
  printf "\n"

let part1 input =
  let towels, designs = parse input in
  (* |> List.map (fun design -> *)
  (*        let _ = printf "\nDesign: %s%!\n" (cltos design) in *)
  (*        find_combinations towels design |> inspect print_combo |> List.length) *)
  designs |> List.filter (find_a_combination towels) |> List.length

let _ = part1 test_case |> printf "Test 1: %d\n"
let _ = part1 day_input |> printf "Part 1: %d\n"
