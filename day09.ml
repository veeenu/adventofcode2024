open Common
open Printf
open Scanf

let test_case = {|2333133121414131402|}

exception Unreachable

let parse input =
  let rec take_pairs = function
    | file_block :: space_block :: xs ->
        (file_block, space_block) :: take_pairs xs
    | file_block :: [] -> [ (file_block, 0) ]
    | [] -> []
  in
  let int_of_digit c = int_of_char c - int_of_char '0' in
  input |> String.to_seq |> Seq.map int_of_digit |> List.of_seq |> take_pairs

type content = File of int | Empty

let rec many_of n value = if n > 0 then value :: many_of (n - 1) value else []

let expand id (file_block, space_block) =
  many_of file_block (File id) @ many_of space_block Empty

let dbg l =
  let () =
    Array.iter (function File id -> printf "%d" id | Empty -> printf ".") l
  in
  printf "\n"

let part1 input =
  let fs = List.mapi expand input |> List.flatten |> Array.of_list in

  let swap a b =
    let v = fs.(a) in
    let () = fs.(a) <- fs.(b) in
    let () = fs.(b) <- v in
    ()
  in

  let rec swap_last_with i n =
    if i < n then (
      match fs.(n) with
      | Empty -> swap_last_with i (n - 1)
      | File _ ->
          swap i n;
          n)
    else n
  in

  let () = dbg fs in

  let rec swap_loop i n =
    if i < n then
      let () = printf "%d %d\n" i n in
      match fs.(i) with
      | File _ -> swap_loop (i + 1) n
      | Empty -> swap_loop (i + 1) (swap_last_with i n)
    else ()
  in

  let () = swap_loop 0 (Array.length fs - 1) in

  let () = dbg fs in

  let checksum =
    let checksum_value idx = function File id -> idx * id | Empty -> 0 in
    Array.mapi checksum_value fs |> Array.fold_left ( + ) 0
  in

  checksum

let () = printf "Test 1: %d\n" (parse test_case |> part1)
let () = printf "Part 1: %d\n" (parse (read_day_lines 9 |> List.hd) |> part1)
