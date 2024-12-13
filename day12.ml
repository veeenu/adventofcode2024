open Common
open Printf

let test_case =
  {|
RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE
|}
  |> String.trim |> String.split_on_char '\n'

let color c =
  let code = (int_of_char c - int_of_char 'A' + 1) mod 26 in
  let i = code * 256 / 26 in
  Printf.sprintf "\x1b[38;2;%d;%d;%dm%c" (i mod 256)
    (i * 2 mod 256)
    (i * 3 mod 256)
    c

let color_print input =
  input
  |> List.iter (fun line ->
         let () =
           String.to_seq line |> Seq.map color
           |> Seq.iter (fun c -> printf "%s" c)
         in
         printf "\x1b[0m%!\n")

module PosSet = Set.Make (struct
  type t = int * int

  let compare (x1, y1) (x2, y2) = compare (y1, x1) (y2, x2)
end)

module Day12 (AocInput : AocInput) = struct
  let grid : char Grid.t = AocInput.input |> Grid.of_lines (fun x -> x)
  let north x y = (x, y - 1)
  let south x y = (x, y + 1)
  let west x y = (x - 1, y)
  let east x y = (x + 1, y)

  let find_cluster clusters x y c =
    List.exists
      (fun (cc, cluster) -> c = cc && Hashtbl.mem cluster (x, y))
      clusters

  let rec flood_fill cluster x y c =
    let () = Hashtbl.replace cluster (x, y) c in
    [ north; south; west; east ]
    |> List.map (fun f -> f x y)
    |> List.filter (fun (x, y) ->
           Grid.is_in_bounds grid x y
           && Grid.at grid x y = c
           && not (Hashtbl.mem cluster (x, y)))
    |> List.iter (fun (x, y) -> flood_fill cluster x y c)

  let clusters =
    Grid.items grid
    |> List.fold_left
         (fun acc (x, y, c) ->
           if find_cluster acc x y c then acc
           else
             let cluster = Hashtbl.create 10 in
             let () = flood_fill cluster x y c in
             (c, cluster) :: acc)
         []

  let () = color_print AocInput.input

  let perimeter cluster =
    let boundaries (x, y) =
      [ north; south; east; west ]
      |> List.map (fun f -> f x y)
      |> List.filter (fun p -> Hashtbl.mem cluster p |> not)
      |> List.length
    in
    Hashtbl.fold (fun k _ acc -> acc + boundaries k) cluster 0

  let area = Hashtbl.length

  type run = Both | Entering | Leaving

  let sides cluster =
    let s = Hashtbl.to_seq cluster |> Seq.map (fun (k, _) -> k) in
    let minmax =
      Seq.fold_left (fun (mina, maxa) b -> (min mina b, max maxa b)) (max_int, 0)
    in
    let min_x, max_x = s |> Seq.map fst |> minmax in
    let min_y, max_y = s |> Seq.map snd |> minmax in
    let mem = Hashtbl.mem cluster in
    let switch_type (p, q) =
      match (mem p, mem q) with
      | true, true | false, false -> Both
      | true, false -> Leaving
      | false, true -> Entering
    in
    let count_switch = function Leaving | Entering -> 1 | Both -> 0 in
    let count_runs curs nexts =
      let () = assert (List.length curs = List.length nexts) in
      let rec count_runs_inner curs nexts switch =
        match (curs, nexts) with
        | [], [] -> 0
        | cur :: curs, next :: nexts ->
            let switch2 = switch_type (cur, next) in
            if switch != switch2 then
              count_switch switch2 + count_runs_inner curs nexts switch2
            else count_runs_inner curs nexts switch2
        | _ -> raise Unreachable
      in
      match (curs, nexts) with
      | [], [] -> 0
      | cur :: curs, next :: nexts ->
          let switch = switch_type (cur, next) in
          count_switch switch + count_runs_inner curs nexts switch
      | _ -> raise Unreachable
    in
    let count_runs_col x =
      let col1, col2 =
        range_up (min_y - 1) (max_y - min_y + 2)
        |> List.map (fun y -> ((x, y), (x + 1, y)))
        |> List.split
      in
      count_runs col1 col2
    in
    let count_runs_row y =
      let row1, row2 =
        range_up (min_x - 1) (max_x - min_x + 2)
        |> List.map (fun x -> ((x, y), (x, y + 1)))
        |> List.split
      in
      count_runs row1 row2
    in
    let sides_x =
      range_up (min_x - 1) (max_x - min_x + 2)
      |> List.map count_runs_col |> List.fold_left ( + ) 0
    in
    let sides_y =
      range_up (min_y - 1) (max_y - min_y + 2)
      |> List.map count_runs_row |> List.fold_left ( + ) 0
    in
    (sides_x, sides_y)

  let measurements =
    clusters |> List.map snd
    |> List.map (fun cluster ->
           let area = area cluster in
           let perimeter = perimeter cluster in
           let sides_x, sides_y = sides cluster in
           (area, perimeter, sides_x + sides_y))

  let part1 =
    measurements
    |> List.fold_left
         (fun acc (area, perimeter, _) -> acc + (area * perimeter))
         0

  let part2 =
    measurements
    |> List.fold_left (fun acc (area, _, sides) -> acc + (area * sides)) 0
end

module TestCase = Day12 (struct
  let input = test_case
end)

module DayInput = Day12 (struct
  let input = read_day_lines 12
end)

let () = printf "Test 1: %d%!\n" TestCase.part1
let () = printf "Part 1: %d%!\n" DayInput.part1
let () = printf "Test 2: %d%!\n" TestCase.part2
let () = printf "Part 2: %d%!\n" DayInput.part2
