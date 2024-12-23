open Common
open Printf
open Scanf

let test_case =
  {|
kh-tc
qp-kh
de-cg
ka-co
yn-aq
qp-ub
cg-tb
vc-aq
tb-ka
wh-tc
yn-cg
kh-ub
ta-co
de-co
tc-td
tb-wq
wh-td
ta-ka
td-qp
aq-cg
wq-ub
ub-vc
de-ta
wq-aq
wq-vc
wh-yn
ka-de
kh-ta
co-tc
wh-qp
tb-vc
td-yn
|}
  |> String.trim |> String.split_on_char '\n'

let day_input = read_day_lines 23

module ComputerMap = Map.Make (struct
  type t = char * char

  let compare = compare
end)

module ComputerSet = Set.Make (struct
  type t = (char * char) * (char * char) * (char * char)

  let compare = compare
end)

let parse input =
  input
  |> List.map (fun s -> s |> String.to_seq |> List.of_seq |> List.nth)
  |> List.map (fun g -> ((g 0, g 1), (g 3, g 4)))

let print ((k1, k2), (v1, v2), (w1, w2)) =
  printf "%c%c,%c%c,%c%c\n" k1 k2 v1 v2 w1 w2

let add_adj src dst =
  ComputerMap.update src (function
    | Some l -> Some (dst :: l)
    | None -> Some (dst :: []))

let graph =
  List.fold_left
    (fun acc (src, dst) -> acc |> add_adj src dst |> add_adj dst src)
    ComputerMap.empty

let triple_has_t (v1, v2, v3) = fst v1 = 't' || fst v2 = 't' || fst v3 = 't'

let sort_triple v1 v2 v3 =
  [ v1; v2; v3 ] |> List.sort compare |> function
  | [ v1; v2; v3 ] -> (v1, v2, v3)
  | _ -> raise Unreachable

let filter_triple g v1 v2 v3 =
  let unwrap_or_false = function Some c -> c | None -> false in
  let has va vb =
    ComputerMap.find_opt va g |> Option.map (List.mem vb) |> unwrap_or_false
  in
  if v1 <> v2 && v2 <> v3 && v1 <> v3 && has v1 v2 && has v2 v3 && has v1 v3
  then Some (sort_triple v1 v2 v3)
  else None

let triples g =
  let adjacency =
    g |> ComputerMap.to_list
    |> List.map (fun (v, edges) -> List.map (fun edge -> (v, edge)) edges)
    |> List.flatten
  in
  let triples =
    adjacency
    |> List.filter_map (fun (v1, v2) ->
           ComputerMap.find_opt v2 g
           |> Option.map (List.filter_map (fun v3 -> filter_triple g v1 v2 v3)))
    |> List.flatten
  in
  triples |> ComputerSet.of_list |> ComputerSet.filter triple_has_t

let part1 input = input |> parse |> graph |> triples |> ComputerSet.cardinal
let _ = part1 test_case |> printf "Test 1: %d\n"
let _ = part1 day_input |> printf "Part 1: %d\n"
