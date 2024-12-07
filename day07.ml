open Common
open Printf
open Scanf

let test_case =
  {|
190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20
|}
  |> String.trim |> String.split_on_char '\n'

let day_input = read_day_lines 7

let parse line =
  let tokens = String.split_on_char ' ' line in
  let sum = sscanf (List.hd tokens) "%d:" (fun d -> d) in
  let numbers = List.map int_of_string (List.tl tokens) in
  (sum, numbers)

type operator = Sum | Mul

let rec prod set n =
  if n = 0 then [ [] ]
  else
    List.map (fun ops -> List.map (fun s -> s :: ops) set) (prod set (n - 1))
    |> List.flatten

let ops_set numbers = prod [ Sum; Mul ] (List.length numbers - 1)

let apply numbers ops =
  let hd = List.hd numbers in
  let tl = List.tl numbers in
  List.fold_left2
    (fun acc op num -> match op with Sum -> acc + num | Mul -> acc * num)
    hd ops tl

let inspect fn list =
  List.map
    (fun x ->
      fn x;
      x)
    list

let dbg_op_list l = List.map (function Sum -> "<sum>" | Mul -> "<mul>") l

let find_matches (sum, numbers) =
  ops_set numbers |> List.map (apply numbers) |> List.map (( = ) sum)

let check_equation (sum, numbers) =
  let equation_valid =
    find_matches (sum, numbers) |> List.fold_left ( || ) false
  in
  if equation_valid then sum else 0

let part1 input =
  input |> List.map parse |> List.map check_equation |> List.fold_left ( + ) 0

let () = assert (part1 test_case = 3749)
let () = printf "Part 1: %d\n" (part1 day_input)
