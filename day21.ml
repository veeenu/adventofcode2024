open Common
open Printf
open Scanf

let test_case =
  {|
029A
980A
179A
456A
379A
|} |> String.trim |> String.split_on_char '\n'

let day_input = read_day_lines 21

type numpad = N0 | N1 | N2 | N3 | N4 | N5 | N6 | N7 | N8 | N9 | NA
type dirpad = Up | Left | Down | Right | DA

let numpad_of_char = function
  | '0' -> N0
  | '1' -> N1
  | '2' -> N2
  | '3' -> N3
  | '4' -> N4
  | '5' -> N5
  | '6' -> N6
  | '7' -> N7
  | '8' -> N8
  | '9' -> N9
  | 'A' -> NA
  | _ -> raise Unreachable

let string_of_numpad = function
  | N0 -> "0"
  | N1 -> "1"
  | N2 -> "2"
  | N3 -> "3"
  | N4 -> "4"
  | N5 -> "5"
  | N6 -> "6"
  | N7 -> "7"
  | N8 -> "8"
  | N9 -> "9"
  | NA -> "A"

let string_of_dirpad = function
  | Up -> "^"
  | Down -> "v"
  | Left -> "<"
  | Right -> ">"
  | DA -> "A"

let parse =
  List.map (fun line ->
      let numpads =
        String.to_seq line |> Seq.map numpad_of_char |> List.of_seq
      in
      let number = sscanf line "%03dA" (fun x -> x) in
      (numpads, number))

(*
    +---+---+
    | ^ | A |
+---+---+---+
| < | v | > |
+---+---+---+ 
*)

let dirpad_graph = function
  | DA, Left -> Some Up
  | DA, Down -> Some Right
  | Up, Right -> Some DA
  | Up, Down -> Some Down
  | Left, Right -> Some Down
  | Down, Left -> Some Left
  | Down, Up -> Some Up
  | Down, Right -> Some Right
  | Right, Left -> Some Down
  | Right, Up -> Some DA
  | _ -> None

let dirpad_graph_inv = function
  | DA, Up -> Some Left
  | DA, Right -> Some Down
  | Up, DA -> Some Right
  | Up, Down -> Some Down
  | Left, Down -> Some Right
  | Down, Left -> Some Left
  | Down, Up -> Some Up
  | Down, Right -> Some Right
  | Right, Down -> Some Left
  | Right, DA -> Some Up
  | _ -> None

(*
+---+---+---+
| 7 | 8 | 9 |
+---+---+---+
| 4 | 5 | 6 |
+---+---+---+
| 1 | 2 | 3 |
+---+---+---+
    | 0 | A |
    +---+---+
 *)

let numpad_graph = function
  | N7, Right -> Some N8
  | N7, Down -> Some N4
  | N8, Left -> Some N7
  | N8, Right -> Some N9
  | N8, Down -> Some N5
  | N9, Left -> Some N8
  | N9, Down -> Some N6
  | N4, Up -> Some N7
  | N4, Right -> Some N5
  | N4, Down -> Some N1
  | N5, Up -> Some N8
  | N5, Left -> Some N4
  | N5, Right -> Some N6
  | N5, Down -> Some N2
  | N6, Up -> Some N9
  | N6, Left -> Some N5
  | N6, Down -> Some N3
  | N1, Up -> Some N4
  | N1, Right -> Some N2
  | N2, Up -> Some N5
  | N2, Left -> Some N1
  | N2, Right -> Some N3
  | N2, Down -> Some N0
  | N3, Up -> Some N6
  | N3, Left -> Some N2
  | N3, Down -> Some NA
  | N0, Up -> Some N2
  | N0, Right -> Some NA
  | NA, Up -> Some N3
  | NA, Left -> Some N0
  | _ -> None

let numpad_graph_inv = function
  | N7, N8 -> Some Right
  | N7, N4 -> Some Down
  | N8, N7 -> Some Left
  | N8, N9 -> Some Right
  | N8, N5 -> Some Down
  | N9, N8 -> Some Left
  | N9, N6 -> Some Down
  | N4, N7 -> Some Up
  | N4, N5 -> Some Right
  | N4, N1 -> Some Down
  | N5, N8 -> Some Up
  | N5, N4 -> Some Left
  | N5, N6 -> Some Right
  | N5, N2 -> Some Down
  | N6, N9 -> Some Up
  | N6, N5 -> Some Left
  | N6, N3 -> Some Down
  | N1, N4 -> Some Up
  | N1, N2 -> Some Right
  | N2, N5 -> Some Up
  | N2, N1 -> Some Left
  | N2, N3 -> Some Right
  | N2, N0 -> Some Down
  | N3, N6 -> Some Up
  | N3, N2 -> Some Left
  | N3, NA -> Some Down
  | N0, N2 -> Some Up
  | N0, NA -> Some Right
  | NA, N3 -> Some Up
  | NA, N0 -> Some Left
  | _ -> None

let bfs graph start target =
  let shortest_path_length paths =
    paths |> List.map List.length |> List.fold_left min max_int
  in
  let rec aux queue paths =
    match queue with
    | [] -> paths
    | (key, path) :: xs ->
        if List.mem key path || List.length path >= shortest_path_length paths
        then aux xs paths
        else if key = target then aux xs ((key :: path) :: paths)
        else
          let neighbors =
            [ Up; Left; Down; Right ]
            |> List.filter_map (fun d -> graph (key, d))
            |> List.map (fun n -> (n, key :: path))
          in
          aux (xs @ neighbors) paths
  in
  if start = target then [] else aux [ (start, []) ] [] |> List.map List.rev

let shortest_paths_numpad =
  let all_numpad_keys = [ N0; N1; N2; N3; N4; N5; N6; N7; N8; N9; NA ] in
  let convert_to_dirpad path =
    (path, (path |> pairs |> List.filter_map numpad_graph_inv) @ [ DA ])
  in
  all_numpad_keys
  |> List.map (fun k1 -> all_numpad_keys |> List.map (fun k2 -> (k1, k2)))
  |> List.flatten
  |> List.map (fun (k1, k2) -> ((k1, k2), bfs numpad_graph k1 k2))
  |> List.map (fun (k, v) -> (k, List.map convert_to_dirpad v))

let shortest_paths_dirpad =
  let all_dirpad_keys = [ DA; Up; Left; Right; Down ] in
  let convert_to_dirpad path =
    (path, (path |> pairs |> List.filter_map dirpad_graph_inv) @ [ DA ])
  in
  all_dirpad_keys
  |> List.map (fun k1 -> all_dirpad_keys |> List.map (fun k2 -> (k1, k2)))
  |> List.flatten
  |> List.map (fun (k1, k2) -> ((k1, k2), bfs dirpad_graph k1 k2))
  |> List.map (fun (k, v) -> (k, List.map convert_to_dirpad v))

let print_path shortest_paths string_of n1 n2 =
  shortest_paths
  |> List.assoc (n1, n2)
  |> List.iter (fun (path, steps) ->
         let _ = path |> List.map string_of |> List.iter (printf "%s ") in
         let _ = printf "-> " in
         let _ =
           steps |> List.map string_of_dirpad |> List.iter (printf "%s ")
         in
         let _ = printf "\n" in
         ())

let print_numpad_path = print_path shortest_paths_numpad string_of_numpad
let print_dirpad_path = print_path shortest_paths_dirpad string_of_dirpad

let find_shortest_path depth n1 n2 =
  let paths = shortest_paths_numpad |> List.assoc (n1, n2) |> List.map snd in
  let memo = Hashtbl.create 10 in
  let rec aux depth k =
    match Hashtbl.find_opt memo (depth, k) with
    | Some c -> c
    | None ->
        let aux_inner depth k =
          if depth = 0 then 1
          else
            let paths = shortest_paths_dirpad |> List.assoc k |> List.map snd in
            let subpaths =
              paths
              |> List.map (fun path ->
                     DA :: path |> pairs
                     |> List.map (aux (depth - 1))
                     |> List.fold_left ( + ) 0)
            in
            match subpaths with [] -> 1 | c -> List.fold_left min max_int c
        in
        let c = aux_inner depth k in
        let _ = Hashtbl.replace memo (depth, k) c in
        c
  in
  paths
  |> List.map (fun path ->
         DA :: path |> pairs |> List.map (aux depth) |> List.fold_left ( + ) 0)
  |> List.fold_left min max_int

let find count keys =
  NA :: keys |> pairs
  |> List.map (fun (n1, n2) -> find_shortest_path count n1 n2)
  |> List.fold_left ( + ) 0

let compute_complexity count input =
  input
  |> List.map (fun (numpad, num) -> num * find count numpad)
  |> List.fold_left ( + ) 0

let part1 input = input |> parse |> compute_complexity 2
let part2 input = input |> parse |> compute_complexity 25
let _ = part1 test_case |> printf "Test 1: %d%!\n"
let _ = part1 day_input |> printf "Part 1: %d%!\n"
let _ = part2 test_case |> printf "Test 2: %d%!\n"
let _ = part2 day_input |> printf "Part 2: %d%!\n"
