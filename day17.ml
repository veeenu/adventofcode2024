open Common
open Printf
open Scanf
open Int64

let test_case =
  {|
Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0
|}
  |> String.trim |> String.split_on_char '\n'

let test_case2 =
  {|
Register A: 2024
Register B: 0
Register C: 0

Program: 0,3,5,4,3,0
|}
  |> String.trim |> String.split_on_char '\n'

let day_input = read_day_lines 17

let parse input =
  let reg_a, input = (List.hd input, List.tl input) in
  let reg_a = sscanf reg_a "Register A: %Lu" (fun x -> x) in
  let reg_b, input = (List.hd input, List.tl input) in
  let reg_b = sscanf reg_b "Register B: %Lu" (fun x -> x) in
  let reg_c, input = (List.hd input, List.tl input) in
  let reg_c = sscanf reg_c "Register C: %Lu" (fun x -> x) in
  let program = input |> List.tl |> List.hd in
  let program =
    String.sub program 9 (String.length program - 9)
    |> String.split_on_char ',' |> List.map of_string
  in
  (program, reg_a, reg_b, reg_c)

let combo (v : int64) (a : int64) (b : int64) (c : int64) =
  match to_int v with
  | 0 | 1 | 2 | 3 -> v
  | 4 -> a
  | 5 -> b
  | 6 -> c
  | _ -> raise Unreachable

let adv op a b c =
  (shift_right_logical a (combo op a b c |> to_int), b, c, None, None)

let bxl op a b c = (a, logxor b op, c, None, None)
let bst op a b c = (a, logand (combo op a b c) 7L, c, None, None)

let jnz op a b c =
  if a = 0L then (a, b, c, None, None) else (a, b, c, Some op, None)

let bxc _ a b c = (a, logxor b c, c, None, None)
let out op a b c = (a, b, c, None, Some (logand (combo op a b c) 7L))

let bdv op a b c =
  (a, shift_right_logical a (combo op a b c |> to_int), c, None, None)

let cdv op a b c =
  (a, b, shift_right_logical a (combo op a b c |> to_int), None, None)

let opcode inst =
  match to_int inst with
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
  match to_int inst with
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
      let () = printf "%s %d\n" (string_of_opcode inst) (to_int op) in
      disasm xs
  | _ -> ()

let rec interpret (program : int64 list) ip a b c =
  if ip + 1 >= List.length program then ()
  else
    let inst = List.nth program ip in
    let op = List.nth program (ip + 1) in
    let a, b, c, jmp, output = (opcode inst) op a b c in
    let _ = output |> function Some x -> printf "%Lu," x | None -> () in
    match jmp with
    | None -> interpret program (ip + 2) a b c
    | Some ip -> interpret program (to_int ip) a b c

let part1 input =
  let program, a, b, c = parse input in
  let _ = interpret program 0 a b c in
  ()

let interpret2 (program : int64 list) a b c =
  let rec interpret_step ip a b c xs =
    if ip + 1 >= List.length program then (a, b, c, xs)
    else
      let inst = List.nth program ip in
      let op = List.nth program (ip + 1) in
      let a, b, c, jmp, output = (opcode inst) op a b c in
      let xs = output |> function Some x -> x :: xs | None -> xs in
      match jmp with
      | None -> interpret_step (ip + 2) a b c xs
      | Some ip -> interpret_step (to_int ip) a b c xs
  in
  interpret_step 0 a b c []

let rinterpret program =
  let rec rinterpret_step a b c count =
    let candidates =
      [ 0L; 1L; 2L; 3L; 4L; 5L; 6L; 7L ]
      |> List.map (fun offset ->
             (offset, interpret2 program (add a offset) b c))
      |> List.filter (fun (_, (_, _, _, result)) ->
             List.nth program count = (result |> List.rev |> List.hd))
    in
    if count = 0 then candidates |> List.map (fun (offset, _) -> add a offset)
    else
      candidates
      |> List.map (fun (offset, (_, b, c, _)) ->
             rinterpret_step (shift_left (add a offset) 3) b c (count - 1))
      |> List.flatten
  in
  rinterpret_step 0L 0L 0L (List.length program - 1)

let () = parse test_case |> fun (program, _, _, _) -> disasm program
let () = printf "\nTest 1: "
let () = part1 test_case
let () = printf "\n\n"
let () = parse day_input |> fun (program, _, _, _) -> disasm program
let () = printf "\nPart 1: "
let () = part1 day_input
let () = printf "\n\n"
let () = parse test_case2 |> fun (program, _, _, _) -> disasm program

let part2 input =
  let program, _, _, _ = parse input in
  let outcome = rinterpret program in
  List.fold_left min max_int outcome |> to_int

let () = part2 test_case2 |> printf "Test 2: %d\n"
let () = part2 day_input |> printf "Part 2: %d\n"
