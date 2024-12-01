let lines : string list =
  let fp = open_in "_input/day01.txt" in
  let rec read_lines acc =
    try
      let line = input_line fp in
      read_lines (line :: acc)
    with End_of_file -> List.rev acc
  in
  read_lines []

let parse line : (int * int) option =
  match String.split_on_char ' ' line |> List.filter (fun x -> x <> "") with
  | [ fst; snd ] ->
      Some (String.trim fst |> int_of_string, String.trim snd |> int_of_string)
  | [] -> None
  | _ -> raise (Failure (Printf.sprintf "Invalid line `%s`" line))

let numbers = List.map parse lines |> List.filter_map (fun x -> x)
let list1 = List.map fst numbers |> List.sort compare
let list2 = List.map snd numbers |> List.sort compare

(* Part 1 *)

let part1 =
  List.combine list1 list2
  |> List.map (fun (a, b) -> b - a |> abs)
  |> List.fold_left ( + ) 0

let () = Printf.printf "Part1: %d\n" part1

(* Part 2 *)

let incmap map value =
  Hashtbl.find_opt map value |> Option.value ~default:0
  |> (fun x -> x + 1)
  |> Hashtbl.replace map value;
  map

let occurrences =
  List.fold_left incmap (Hashtbl.create (List.length list2)) list2

let score_fn x =
  Hashtbl.find_opt occurrences x
  |> Option.map (fun y -> y * x)
  |> Option.value ~default:0

let score = List.map score_fn list1 |> List.fold_left ( + ) 0

let () = Printf.printf "Part2: %d\n" score
