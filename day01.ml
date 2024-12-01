open Common

let parse line =
  String.split_on_char ' ' line
  |> List.filter_map (function
       | "" -> None
       | x -> Some (String.trim x |> int_of_string))
  |> function
  | [ x1; x2 ] -> Some (x1, x2)
  | [] -> None
  | _ -> raise (Failure (Printf.sprintf "Invalid line `%s`" line))

let list1, list2 =
  read_day_lines 1 |> List.filter_map parse
  |> List.fold_left
       (fun (acc1, acc2) (x1, x2) -> (x1 :: acc1, x2 :: acc2))
       ([], [])
  |> fun (list1, list2) -> (List.sort compare list1, List.sort compare list2)

(* Part 1 *)

let part1 =
  List.combine list1 list2
  |> List.map (fun (a, b) -> b - a |> abs)
  |> List.map (fun x ->
         Printf.printf "%d\n" x;
         x)
  |> List.fold_left ( + ) 0

let () = Printf.printf "Part1: %d\n" part1

(* Part 2 *)

let occurrences =
  List.fold_left
    (fun map key ->
      Hashtbl.find_opt map key
      |> (function Some x -> x + 1 | None -> 1)
      |> Hashtbl.replace map key;
      map)
    (Hashtbl.create (List.length list2))
    list2

let score_fn x =
  Hashtbl.find_opt occurrences x |> function Some y -> y * x | None -> 0

let part2 = List.map score_fn list1 |> List.fold_left ( + ) 0
let () = Printf.printf "Part2: %d\n" part2
