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

type block = (int * int) * (int * int) * (int * int)

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

let combinations =
  range_up 0 100
  |> List.map (fun x -> range_up 0 100 |> List.map (fun y -> (x, y)))
  |> List.flatten

let winning_combinations ((ax, ay), (bx, by), (px, py)) =
  combinations
  |> List.filter (fun (i, j) ->
         ((i * ax) + (j * bx), (i * ay) + (j * by)) = (px, py))

let combination_cost a b = (a * 3) + b

let best_combination block =
  block |> winning_combinations |> function
  | [] -> None
  | l ->
      Some
        (List.fold_left
           (fun (a, b, cost) (a', b') ->
             let cost' = combination_cost a' b' in
             if cost' < cost then (a', b', cost') else (a, b, cost))
           (0, 0, max_int) l)

let part1 input =
  input |> parse |> List.map best_combination
  |> inspect (function
       | None -> printf "No solution found\n"
       | Some (a, b, cost) -> printf "Cost %d: (%d, %d)\n" cost a b)
  |> List.filter_map (function None -> None | Some (_, _, c) -> Some c)
  |> List.fold_left ( + ) 0

(*
the minimum for X is goign to be PX / max(ax, bx)
the maximum for X is goign to be PX / min(ax, bx)
the minimum for Y is goign to be PY / may(ay, by)
the maximum for Y is goign to be PY / min(ay, by)
 *)

let () = printf "Test 1: %d\n" (part1 test_case)
let () = printf "Part 1: %d\n" (part1 day_input)
