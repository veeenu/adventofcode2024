open Common
open Printf

let test_case =
  {|
#####
.####
.####
.####
.#.#.
.#...
.....

#####
##.##
.#.##
...##
...#.
...#.
.....

.....
#....
#....
#...#
#.#.#
#.###
#####

.....
.....
#.#..
###..
###.#
###.#
#####

.....
.....
.....
#....
#.#..
#.#.#
#####
|}
  |> String.trim |> String.split_on_char '\n'

let day_input = read_day_lines 25

let parse input =
  let rec parse_block blocks input =
    match input with
    | a1 :: a2 :: a3 :: a4 :: a5 :: a6 :: a7 :: xs ->
        parse_block ([ a1; a2; a3; a4; a5; a6; a7 ] :: blocks) xs
    | _ -> blocks
  in
  input |> List.map String.trim
  |> List.filter (( <> ) String.empty)
  |> parse_block [] |> List.rev
  |> List.partition (fun block ->
         List.nth block 0 |> String.to_seq |> Seq.for_all (fun c -> c = '#'))

let to_num block =
  range_up 0 5
  |> List.map (fun col ->
         block
         |> List.map (fun row -> String.get row col)
         |> List.filter (( = ) '#')
         |> List.length
         |> fun i -> i - 1)

let part1 input =
  let locks, keys = parse input in
  let locks = locks |> List.map to_num in
  let keys = keys |> List.map to_num in
  let combinations =
    locks
    |> List.map (fun lock -> keys |> List.map (fun key -> (lock, key)))
    |> List.flatten
  in
  combinations
  |> List.filter (fun (lock, key) ->
         List.map2 (fun l k -> l + k) lock key |> List.for_all (( >= ) 5))
  |> List.length

let _ = part1 test_case |> printf "Test 1: %d\n"
let _ = part1 day_input |> printf "Part 1: %d\n"
