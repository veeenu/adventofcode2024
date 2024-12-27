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

let test_case3 =
  {|
x00: 0
x01: 1
x02: 0
x03: 1
x04: 0
x05: 1
y00: 0
y01: 0
y02: 1
y03: 1
y04: 0
y05: 1

x00 AND y00 -> z05
x01 AND y01 -> z02
x02 AND y02 -> z01
x03 AND y03 -> z03
x04 AND y04 -> z04
x05 AND y05 -> z00
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

let sprint_triple (a1, a2, a3) = sprintf "%c%c%c" a1 a2 a3
let print_triple t = printf "%s\n" (sprint_triple t)

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
  if TripleSet.cardinal zees = TripleSet.cardinal next_zees then next_values
  else apply_ops next_values operations zees

let swap_ops g1 g2 operations =
  let swap_ops_idx i1 i2 =
    let op1a, op1b, op1out, op1op = List.nth operations i1 in
    let op2a, op2b, op2out, op2op = List.nth operations i2 in
    let op1_swapped = (op1a, op1b, op2out, op1op) in
    let op2_swapped = (op2a, op2b, op1out, op2op) in
    operations
    |> List.mapi (fun i op ->
           if i = i1 then op1_swapped else if i = i2 then op2_swapped else op)
  in
  let i1 =
    List.find_index (fun (_, _, c, _) -> c = g1) operations |> Option.get
  in
  let i2 =
    List.find_index (fun (_, _, c, _) -> c = g2) operations |> Option.get
  in
  swap_ops_idx i1 i2

let rec pow2 exp = if exp = 0 then 1 else 2 * pow2 (exp - 1)

let collect_bits c values =
  values |> TripleMap.to_list
  |> List.filter (fun ((k1, _, _), _) -> k1 = c)
  |> List.sort (fun (a, _) (b, _) -> compare a b)
  |> List.map snd

let collect c values =
  collect_bits c values
  |> List.mapi (fun i v -> v * pow2 i)
  |> List.fold_left ( + ) 0

let part1 input =
  let values, operations, zees = parse input in
  let values = apply_ops values operations zees in
  collect 'z' values

let convert_to_dot operations =
  let edges =
    operations
    |> List.map (fun (src1, src2, dst, op) ->
           let color =
             match op with And -> "red" | Xor -> "black" | Or -> "green"
           in
           [
             sprintf "%s -> %s [color=\"%s\"]" (sprint_triple src1)
               (sprint_triple dst) color;
             sprintf "%s -> %s [color=\"%s\"]" (sprint_triple src2)
               (sprint_triple dst) color;
           ])
    |> List.flatten |> String.concat "\n"
  in
  sprintf "digraph {\n%s\n}" edges

(* dot day24.dot -Tsvg -o day24.svg *)
let save_to_dot operations path =
  let o = open_out path in
  let _ = fprintf o "%s" (convert_to_dot operations) in
  let _ = close_out o in
  ()

let bits_of n =
  let rec aux n acc =
    if n = 0 then acc else aux (n / 2) (string_of_int (n mod 2) ^ acc)
  in
  if n = 0 then "0" else aux n ""

let bits_of n =
  let rec aux n acc = if n = 0 then acc else aux (n / 2) ((n mod 2) :: acc) in
  aux n []

let part2 input =
  let values, operations, zees = parse input in
  let pad_to = TripleSet.cardinal zees in

  let print_bits b =
    let rec pad count s =
      if count <= List.length s then s else "0" :: pad (count - 1) s
    in
    b
    |> List.map (function 0 -> "0" | 1 -> "1" | _ -> raise Unreachable)
    |> pad pad_to |> String.concat ""
  in

  let _ = printf "x: %s\n" (collect_bits 'x' values |> print_bits) in
  let _ = printf "y: %s\n" (collect_bits 'y' values |> print_bits) in

  let x = collect 'x' values in
  let y = collect 'y' values in
  let z_target = x + y in
  let z_calculated = apply_ops values operations zees |> collect 'z' in
  let _ =
    printf "%d + %d = %d\ndst = %s\nsrc = %s\nmsk = %s\n" x y z_target
      (z_target |> bits_of |> print_bits)
      (z_calculated |> bits_of |> print_bits)
      (z_target lxor z_calculated |> bits_of |> print_bits)
  in

  let operations =
    operations
    |> swap_ops ('f', 'd', 'v') ('d', 'b', 'p')
    |> swap_ops ('z', '1', '5') ('c', 'k', 'j')
    |> swap_ops ('k', 'd', 'f') ('z', '2', '3')
    |> swap_ops ('z', '3', '9') ('r', 'p', 'p')
  in
  let _ = save_to_dot operations "_build/day24-chg.dot" in
  let z_calculated = apply_ops values operations zees |> collect 'z' in
  let _ =
    printf "chg = %s\n" (z_target lxor z_calculated |> bits_of |> print_bits)
  in

  [ "fdv"; "dbp"; "z15"; "ckj"; "kdf"; "z23"; "z39"; "rpp" ]
  |> List.sort compare |> String.concat ","

let _ = part1 test_case |> printf "Test 1: %d\n"
let _ = part1 test_case2 |> printf "Test 1: %d\n"
let _ = part1 day_input |> printf "Part 1: %d\n"

let _ =
  let _, operations, _ = parse day_input in
  save_to_dot operations "_build/day24-orig.dot"

let _ = part2 day_input |> printf "Part 2: %s\n"
