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

let rec gcd a b = if b = 0 then a else gcd b (a mod b)
let lcm a b = a * b / gcd a b

let adjust_claw (a, b, (px, py)) =
  (a, b, (px + 10000000000000, py + 10000000000000))

let print ((ax, ay), (bx, by), (px, py)) =
  printf "---\n%di + %dj + %d = 0\n%di + %dj + %d = 0\n\n" ax bx px ay by py

let gauss ((ax, ay), (bx, by), (px, py)) =
  let lcm = lcm ax ay in
  let () = printf "lcm=%d\n" lcm in
  let (ax, ay), (bx, by), (px, py) =
    ((ax * lcm, ay), (bx * lcm, by), (px * lcm, py))
  in
  let () = print ((ax, ay), (bx, by), (px, py)) in
  let (ax, ay), (bx, by), (px, py) =
    let factor = -ax / ay in
    ((ax, ay * factor), (bx, by * factor), (px, py * factor))
  in
  let () = print ((ax, ay), (bx, by), (px, py)) in
  let (ax, ay), (bx, by), (px, py) =
    ((ax, ay + ax), (bx, by + bx), (px, py + px))
  in
  let () = print ((ax, ay), (bx, by), (px, py)) in
  let j = py / by in
  let d = px - (j * bx) in
  let i = d / ax in
  if py mod by = 0 && d mod ax = 0 then Some (i, j) else None

let part1 input =
  input |> parse |> List.filter_map gauss
  |> List.map (fun (a, b) -> (a * 3) + b)
  |> List.fold_left ( + ) 0

let part2 input =
  input |> parse |> List.map adjust_claw |> List.filter_map gauss
  |> List.map (fun (a, b) -> (a * 3) + b)
  |> List.fold_left ( + ) 0

let () = printf "Test 1: %d%!\n" (part1 test_case)
let () = printf "Part 1: %d%!\n" (part1 day_input)
let () = printf "Test 2: %d%!\n" (part2 test_case)
let () = printf "Part 2: %d%!\n" (part2 day_input)
