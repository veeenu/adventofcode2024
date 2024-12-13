open Common
open Printf
open Scanf

let test_case =
  {|
Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279
|}
  |> String.trim |> String.split_on_char '\n'

let day_input = read_day_lines 13

let parse_block button_a button_b prize =
  let ax, ay = sscanf button_a "Button A: X+%d, Y+%d" (fun x y -> (x, y)) in
  let bx, by = sscanf button_b "Button B: X+%d, Y+%d" (fun x y -> (x, y)) in
  let px, py = sscanf prize "Prize: X=%d, Y=%d" (fun x y -> (x, y)) in
  ((ax, ay), (bx, by), (px, py))

let rec parse = function
  | button_a :: button_b :: prize :: _ :: xs ->
      parse_block button_a button_b prize :: parse xs
  | [ button_a; button_b; prize ] -> parse_block button_a button_b prize :: []
  | _ -> raise Unreachable

let combinations min_n max_n =
  range_up min_n (max_n - min_n)
  |> List.map (fun x ->
         range_up min_n (max_n - min_n) |> List.map (fun y -> (x, y)))
  |> List.flatten

let winning_combinations combinations ((ax, ay), (bx, by), (px, py)) =
  combinations
  |> List.filter (fun (i, j) ->
         ((i * ax) + (j * bx), (i * ay) + (j * by)) = (px, py))

let combination_cost a b = (a * 3) + b

let least_costly = function
  | [] -> None
  | l ->
      Some
        (List.fold_left
           (fun (a, b, cost) (a', b') ->
             let cost' = combination_cost a' b' in
             if cost' < cost then (a', b', cost') else (a, b, cost))
           (0, 0, max_int) l)

let best_combination combinations block =
  block |> winning_combinations combinations |> least_costly

let part1 input =
  input |> parse
  |> List.map (best_combination (combinations 0 100))
  |> List.filter_map (function None -> None | Some (_, _, c) -> Some c)
  |> List.fold_left ( + ) 0

let adjust_claw (a, b, (px, py)) =
  (a, b, (px + 10000000000000, py + 10000000000000))

(* let is_winning i j ((ax, ay), (bx, by), (px, py)) = *)
(*   px mod ((i * ax) + (j * bx)) = 0 && py mod ((i * ay) + (j * by)) = 0 *)
(**)
(* let winning_div i j ((ax, ay), (bx, by), (px, py)) = *)
(*   let mi = px / ((i * ax) + (j * bx)) in *)
(*   let mj = py / ((i * ay) + (j * by)) in *)
(*   (mi * i, mj * j) *)

(* let find_winning block = *)
(*   prime_combinations 10000 *)
(*   |> List.filter (fun (i, j) -> i != 0 || j != 0) *)
(*   |> List.filter (fun (i, j) -> is_winning i j block) *)
(*   |> List.map (fun (i, j) -> winning_div i j block) *)
(**)
(* let part2 input = *)
(*   input |> parse |> List.map adjust_claw *)
(*   |> List.filter_map (fun candidates -> *)
(*          find_winning candidates |> least_costly |> function *)
(*          | None -> None *)
(*          | Some (_, _, c) -> Some c) *)
(*   |> List.fold_left ( + ) 0 *)

(*
   we do a smallish combination set but we check whether px is divisible by i ax + j bx
   px mod (i ax + j bx) = 0 -> px / (i ax + j bx) 
 *)

let rec gcd a b = if b = 0 then a else gcd b (a mod b)

let divisors a b =
  let gcd = gcd a b in
  let () = printf "gcd %d%!\n" gcd in
  (* range_up 1 (gcd |> float |> sqrt |> int_of_float) *)
  range_up 1 gcd |> List.filter (fun x -> gcd mod x = 0)

let () = divisors 8400 5400 |> List.iter (fun x -> printf "%d%!\n" x)
(* divisors 10000000008400 10000000005400 |> List.iter (fun x -> printf "%d%!\n" x) *)

(* let () = printf "Test 1: %d%!\n" (part1 test_case) *)
(* let () = printf "Part 1: %d%!\n" (part1 day_input) *)
(* let () = printf "Test 2: %d%!\n" (part2 test_case) *)
(* let () = printf "Part 2: %d%!\n" (part2 day_input) *)
