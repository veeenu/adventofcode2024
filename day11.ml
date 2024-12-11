open Common
open Printf
open Scanf

let test_case = {|125 17|} |> String.trim |> String.split_on_char '\n'
let day_input = read_day_lines 11

exception Unreachable

let parse input =
  input |> List.hd |> String.split_on_char ' '
  |> List.map (fun x -> sscanf x "%d" (fun i -> i))

let rec log10 n = if n > 9 then 1 + log10 (n / 10) else 0
let rec pow10 exp = if exp = 0 then 1 else 10 * pow10 (exp - 1)

let split_digits i =
  let digits = log10 i + 1 in
  if digits mod 2 = 0 then
    let r = pow10 (digits / 2) in
    [ i / r; i mod r ]
  else raise Unreachable

let apply_rules_once i =
  match i with
  | 0 -> [ 1 ]
  | i when (log10 i + 1) mod 2 = 0 -> split_digits i
  | i -> [ i * 2024 ]

let memo : (int * int, int) Hashtbl.t = Hashtbl.create 2024

let rec apply_rules n i =
  Hashtbl.find_opt memo (n, i) |> function
  | Some x -> x
  | None ->
      if n = 0 then 1
      else
        let r =
          apply_rules_once i
          |> List.map (apply_rules (n - 1))
          |> List.fold_left ( + ) 0
        in
        let () = Hashtbl.replace memo (n, i) r in
        r

let apply_all n input =
  input |> List.map (apply_rules n) |> List.fold_left ( + ) 0

let part1 = apply_all 25
let part2 = apply_all 75
let () = printf "Test 1: %d%!\n" (parse test_case |> part1)
let () = printf "Part 1: %d%!\n" (parse day_input |> part1)
let () = printf "Test 2: %d%!\n" (parse test_case |> part2)
let () = printf "Part 2: %d%!\n" (parse day_input |> part2)
