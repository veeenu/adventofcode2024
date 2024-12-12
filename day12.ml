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

  type dir = North | South | West | East

  let cw = function
    | North -> East
    | East -> South
    | South -> West
    | West -> North

  let ccw = function
    | North -> West
    | East -> North
    | South -> East
    | West -> South

  let fwd x = x
  let back d = d |> cw |> cw

  let fun_of_dir = function
    | North -> north
    | East -> east
    | South -> south
    | West -> west

  let char_of_dir = function
    | North -> 'N'
    | South -> 'S'
    | West -> 'W'
    | East -> 'E'

  let move f d x y = (f d, (f d |> fun_of_dir) x y)

  let perimeter cluster =
    let boundaries (x, y) =
      [ north; south; east; west ]
      |> List.map (fun f -> f x y)
      |> List.filter (fun p -> Hashtbl.mem cluster p |> not)
      |> List.length
    in
    Hashtbl.fold (fun k _ acc -> acc + boundaries k) cluster 0

  let area = Hashtbl.length

  let part1 =
    clusters
    |> List.map (fun (c, cluster) ->
           let () = printf "Cluster %c:\n  " c in
           let () =
             Hashtbl.iter (fun (x, y) _ -> printf "(%d %d) " x y) cluster
           in
           let () = printf "\n%!" in
           let area = area cluster in
           let perimeter = perimeter cluster in
           let () =
             printf "  area %d perimeter %d price %d%!\n" area perimeter
               (area * perimeter)
           in
           (area, perimeter))
    |> List.fold_left (fun acc (area, perimeter) -> acc + (area * perimeter)) 0
end

module TestCase = Day12 (struct
  let input = test_case
end)

module DayInput = Day12 (struct
  let input = read_day_lines 12
end)

let () = printf "Test 1: %d%!\n" TestCase.part1
let () = printf "Part 1: %d%!\n" DayInput.part1
