open Common
open Printf
open Scanf

let test_case =
  {|
x00: 1
x01: 1
x02: 1
y00: 0
y01: 1
y02: 0

x00 AND y00 -> z00
x01 XOR y01 -> z01
x02 OR y02 -> z02
|}
  |> String.trim |> String.split_on_char '\n'

let test_case2 =
  {|
x00: 1
x01: 0
x02: 1
x03: 1
x04: 0
y00: 1
y01: 1
y02: 1
y03: 1
y04: 1

ntg XOR fgs -> mjb
y02 OR x01 -> tnw
kwq OR kpj -> z05
x00 OR x03 -> fst
tgd XOR rvg -> z01
vdt OR tnw -> bfw
bfw AND frj -> z10
ffh OR nrd -> bqk
y00 AND y03 -> djm
y03 OR y00 -> psh
bqk OR frj -> z08
tnw OR fst -> frj
gnj AND tgd -> z11
bfw XOR mjb -> z00
x03 OR x00 -> vdt
gnj AND wpb -> z02
x04 AND y00 -> kjc
djm OR pbm -> qhw
nrd AND vdt -> hwm
kjc AND fst -> rvg
y04 OR y02 -> fgs
y01 AND x02 -> pbm
ntg OR kjc -> kwq
psh XOR fgs -> tgd
qhw XOR tgd -> z09
pbm OR djm -> kpj
x03 XOR y03 -> ffh
x00 XOR y04 -> ntg
bfw OR bqk -> z06
nrd XOR fgs -> wpb
frj XOR qhw -> z04
bqk OR frj -> z07
y03 OR x01 -> nrd
hwm AND bqk -> z03
tgd XOR rvg -> z12
tnw OR pbm -> gnj
|}
  |> String.trim |> String.split_on_char '\n'

let day_input = read_day_lines 24

module Triple = struct
  type t = char * char * char

  let compare = compare
end

module TripleMap = Map.Make (Triple)
module TripleSet = Set.Make (Triple)

type op = Or | And | Xor
type triple = char * char * char
type operation_tuple = triple * triple * triple * op

let op_of_string = function
  | "AND" -> And
  | "XOR" -> Xor
  | "OR" -> Or
  | _ -> raise Unreachable

let fn_of_op = function And -> ( land ) | Or -> ( lor ) | Xor -> ( lxor )
let string_of_op = function And -> "&" | Or -> "|" | Xor -> "^"

let print_op ((a1, a2, a3), (b1, b2, b3), (c1, c2, c3), op) =
  printf "%c%c%c %s %c%c%c -> %c%c%c\n" a1 a2 a3 (string_of_op op) b1 b2 b3 c1
    c2 c3

let print_triple (a1, a2, a3) = printf "%c%c%c\n" a1 a2 a3

let parse input =
  let is_not_empty s = String.trim s <> "" in
  let init = input |> take_while is_not_empty in
  let ops = input |> drop_while is_not_empty in
  let parse_init i = sscanf i "%c%c%c: %d" (fun a b c i -> ((a, b, c), i)) in
  let parse_cmd cmd =
    sscanf cmd "%c%c%c %s %c%c%c -> %c%c%c" (fun a1 a2 a3 o b1 b2 b3 c1 c2 c3 ->
        ((a1, a2, a3), (b1, b2, b3), (c1, c2, c3), op_of_string o))
  in
  let initial_values = init |> List.map parse_init |> TripleMap.of_list in
  let operations = ops |> List.map parse_cmd in
  let zees =
    operations
    |> List.filter_map (fun (_, _, (c1, c2, c3), _) ->
           if c1 = 'z' then Some (c1, c2, c3) else None)
    |> TripleSet.of_list
  in
  (initial_values, operations, zees)

let rec apply_ops (values : int TripleMap.t) (operations : operation_tuple list)
    (zees : TripleSet.t) =
  let next_values =
    operations
    |> List.filter_map (fun (in1, in2, out, op) ->
           let v1 = TripleMap.find_opt in1 values in
           let v2 = TripleMap.find_opt in2 values in
           let vout = TripleMap.find_opt out values in
           match (v1, v2, vout) with
           | _, _, Some vout -> Some (out, vout)
           | Some v1, Some v2, None -> Some (out, fn_of_op op v1 v2)
           | _ -> None)
  in
  let next_zees =
    next_values
    |> List.filter_map (fun ((a1, a2, a3), _) ->
           if a1 = 'z' then Some (a1, a2, a3) else None)
    |> TripleSet.of_list
  in
  let next_values = next_values |> TripleMap.of_list in
  let _ =
    printf "%d %d\n" (TripleSet.cardinal zees) (TripleSet.cardinal next_zees)
  in
  if TripleSet.cardinal zees = TripleSet.cardinal next_zees then next_values
  else apply_ops next_values operations zees

let rec pow2 exp = if exp = 0 then 1 else 2 * pow2 (exp - 1)

let collect_z values =
  values |> TripleMap.to_list
  |> List.filter (fun ((k1, _, _), _) -> k1 = 'z')
  |> List.sort (fun (a, _) (b, _) -> compare a b)
  |> List.mapi (fun i (_, v) -> v * pow2 i)
  |> List.fold_left ( + ) 0

let part1 input =
  let values, operations, zees = parse input in
  let _ = printf "%!\n" in
  let _ = zees |> TripleSet.to_list |> List.iter print_triple in
  let _ = printf "%!\n" in
  let values = apply_ops values operations zees in
  collect_z values

let _ = part1 test_case |> printf "Test 1: %d\n"
let _ = part1 test_case2 |> printf "Test 1: %d\n"
let _ = part1 day_input |> printf "Part 1: %d\n"
