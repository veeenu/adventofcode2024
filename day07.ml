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

let rec prod set n =
  if n = 0 then [ [] ]
  else
    List.map (fun ops -> List.map (fun s -> s :: ops) set) (prod set (n - 1))
    |> List.flatten

(* Part 1 *)

type operator = Sum | Mul

let ops_set numbers = prod [ Sum; Mul ] (List.length numbers - 1)

let apply numbers ops =
  let hd = List.hd numbers in
  let tl = List.tl numbers in
  List.fold_left2
    (fun acc op num -> match op with Sum -> acc + num | Mul -> acc * num)
    hd ops tl

let find_matches (sum, numbers) =
  ops_set numbers |> List.map (apply numbers) |> List.map (( = ) sum)

let check_equation (sum, numbers) =
  let equation_valid =
    find_matches (sum, numbers) |> List.fold_left ( || ) false
  in
  if equation_valid then sum else 0

let part1 input =
  input |> List.map parse |> List.map check_equation |> List.fold_left ( + ) 0

(* Part 2 *)

type operator2 = Sum | Mul | Cons

let ops_set2 numbers = prod [ Sum; Mul; Cons ] (List.length numbers - 1)

let cons x y =
  let rec log10 n = if n > 9 then 1 + log10 (n / 10) else 0 in
  let rec pow10 exp = if exp = 0 then 1 else 10 * pow10 (exp - 1) in
  x * (pow10 (log10 y)) * 10 + y

let apply2 numbers ops =
  let hd = List.hd numbers in
  let tl = List.tl numbers in
  List.fold_left2
    (fun acc op num ->
      match op with Sum -> acc + num | Mul -> acc * num | Cons -> cons acc num)
    hd ops tl

let find_matches2 (sum, numbers) =
  ops_set2 numbers |> List.map (apply2 numbers) |> List.map (( = ) sum)

let check_equation2 (sum, numbers) =
  let equation_valid =
    find_matches2 (sum, numbers) |> List.fold_left ( || ) false
  in
  if equation_valid then sum else 0

let part2 input =
  input |> List.map parse |> List.map check_equation2 |> List.fold_left ( + ) 0

(* Results *)

let () = assert (part1 test_case = 3749)
let () = printf "Part 1: %d\n" (part1 day_input)
let () = assert (part2 test_case = 11387)
let () = printf "Part 2: %d\n" (part2 day_input)
