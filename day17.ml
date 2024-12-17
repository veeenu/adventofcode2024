open Common
open Printf
open Scanf

let test_case =
  {|
Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0
|}
  |> String.trim |> String.split_on_char '\n'

let day_input = read_day_lines 17

let parse input =
  let reg_a, input = (List.hd input, List.tl input) in
  let reg_a = sscanf reg_a "Register A: %d" (fun x -> x) in
  let reg_b, input = (List.hd input, List.tl input) in
  let reg_b = sscanf reg_b "Register B: %d" (fun x -> x) in
  let reg_c, input = (List.hd input, List.tl input) in
  let reg_c = sscanf reg_c "Register C: %d" (fun x -> x) in
  let program = input |> List.tl |> List.hd in
  let program =
    String.sub program 9 (String.length program - 9)
    |> String.split_on_char ',' |> List.map int_of_string
  in
  (program, reg_a, reg_b, reg_c)

let combo v a b c =
  match v with
  | 0 | 1 | 2 | 3 -> v
  | 4 -> a
  | 5 -> b
  | 6 -> c
  | _ -> raise Unreachable

let rec pow base exp = if exp = 0 then 1 else base * pow base (exp - 1)
let adv op a b c = (a / pow 2 (combo op a b c), b, c, None)
let bxl op a b c = (a, b lxor op, c, None)
let bst op a b c = (a, modulo (combo op a b c) 8, c, None)
let jnz op a b c = if a = 0 then (a, b, c, None) else (a, b, c, Some op)
let bxc _ a b c = (a, b lxor c, c, None)

let out op a b c =
  let () = printf "%d," (modulo (combo op a b c) 8) in
  (a, b, c, None)

let bdv op a b c = (a, a / pow 2 (combo op a b c), c, None)
let cdv op a b c = (a, b, a / pow 2 (combo op a b c), None)

let opcode inst =
  match inst with
  | 0 -> adv
  | 1 -> bxl
  | 2 -> bst
  | 3 -> jnz
  | 4 -> bxc
  | 5 -> out
  | 6 -> bdv
  | 7 -> cdv
  | _ -> raise Unreachable

let string_of_opcode inst =
  match inst with
  | 0 -> "adv"
  | 1 -> "bxl"
  | 2 -> "bst"
  | 3 -> "jnz"
  | 4 -> "bxc"
  | 5 -> "out"
  | 6 -> "bdv"
  | 7 -> "cdv"
  | _ -> raise Unreachable

let rec disasm = function
  | inst :: op :: xs ->
      let () = printf "%s %d\n" (string_of_opcode inst) op in
      disasm xs
  | _ -> ()

let rec interpret program ip a b c =
  if ip + 1 >= List.length program then ()
  else
    let inst = List.nth program ip in
    let op = List.nth program (ip + 1) in
    let a, b, c, jmp = (opcode inst) op a b c in
    match jmp with
    | None -> interpret program (ip + 2) a b c
    | Some ip -> interpret program ip a b c

let part1 input =
  let program, a, b, c = parse input in
  let _ = interpret program 0 a b c in
  ()

let () = parse test_case |> fun (program, _, _, _) -> disasm program
let () = printf "\nTest 1: "
let () = part1 test_case
let () = printf "\n\n"
let () = parse day_input |> fun (program, _, _, _) -> disasm program
let () = printf "\nPart 1: "
let () = part1 day_input
