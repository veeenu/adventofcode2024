open Common
open Printf
open Scanf

let test_case =
  {|
47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47
  |}

module Day05 (DayInput : sig
  val input : string list
end) =
struct
  open DayInput

  let split_input lst =
    let rec aux acc = function
      | [] -> (List.rev acc, [])
      | x :: xs ->
          if String.empty = x then (List.rev acc, xs) else aux (x :: acc) xs
    in
    aux [] lst

  let split_edges xs =
    List.map (fun x -> sscanf x "%d|%d" (fun src dst -> (src, dst))) xs

  let split_paths xs =
    List.map (fun x -> String.split_on_char ',' x |> List.map int_of_string) xs

  let edges, paths =
    input |> split_input |> function
    | edges, paths -> (split_edges edges, split_paths paths)

  let is_before x x' =
    edges |> List.exists (fun (src, dst) -> src = x && dst = x')

  let are_before x xs = List.for_all (fun y -> is_before x y) xs

  let rec is_ordered = function
    | x :: xs -> are_before x xs && is_ordered xs
    | [] -> true

  let middle xs = List.nth xs (List.length xs / 2)
  let middle_ordered_values = List.filter is_ordered paths |> List.map middle
  let part1 = List.fold_left ( + ) 0 middle_ordered_values
end

module TestCase = Day05 (struct
  let input = test_case |> String.trim |> String.split_on_char '\n'
end)

module DayInput = Day05 (struct
  let input = read_day_lines 5
end)

let () = printf "Part 1: %d\n" DayInput.part1
