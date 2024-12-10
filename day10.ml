open Common
open Printf

let test_case =
  {|
89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732
|}
  |> String.trim |> String.split_on_char '\n'

module Day10 (AocInput : AocInput) = struct
  open AocInput

  let grid = input |> Grid.of_lines (fun c -> int_of_char c - int_of_char '0')

  let trails_memo : (int * int, (int * int) list list) Hashtbl.t =
    Hashtbl.create 1024

  let north x y = (x, y - 1)
  let south x y = (x, y + 1)
  let east x y = (x + 1, y)
  let west x y = (x - 1, y)

  let rec descend_trailheads xsrc ysrc =
    match Hashtbl.find_opt trails_memo (xsrc, ysrc) with
    | Some trailheads -> trailheads
    | None when Grid.at grid xsrc ysrc = 0 -> [ [ (xsrc, ysrc) ] ]
    | None ->
        let ths =
          [ north xsrc ysrc; south xsrc ysrc; east xsrc ysrc; west xsrc ysrc ]
          |> List.filter (fun (x, y) ->
                 Grid.is_in_bounds grid x y
                 && Grid.at grid x y = Grid.at grid xsrc ysrc - 1)
          |> List.map (fun (xstep, ystep) ->
                 descend_trailheads xstep ystep
                 |> List.map (fun l -> (xsrc, ysrc) :: l))
          |> List.flatten
        in
        let () = Hashtbl.replace trails_memo (xsrc, ysrc) ths in
        ths

  let () =
    Grid.items grid
    |> List.filter (fun (_, _, v) -> v = 9)
    |> List.iter (fun (x, y, _) ->
           let _ = descend_trailheads x y in
           ())

  let trails =
    Hashtbl.to_seq trails_memo |> List.of_seq |> List.map snd |> List.flatten
    |> List.filter (fun l -> List.length l = 10)
    |> List.map List.rev

  let trailheadstails =
    trails
    |> List.map (fun l -> (List.hd l, List.hd (List.rev l)))
    |> List.sort_uniq comparexy
    |> List.sort (fun (a, _) (b, _) -> comparexy a b)

  let trailheads_scores =
    trailheadstails
    |> List.fold_left
         (fun acc (trailhead, _) ->
           let () =
             Hashtbl.replace acc trailhead
               (match Hashtbl.find_opt acc trailhead with
               | Some score -> score + 1
               | None -> 1)
           in
           acc)
         (Hashtbl.create 10)
    |> Hashtbl.to_seq

  let part1 =
    trailheads_scores |> List.of_seq |> List.map snd |> List.fold_left ( + ) 0

  let trailheads_ratings =
    trails
    |> List.fold_left
         (fun acc trail ->
           let trailhead = List.hd trail in
           let () =
             Hashtbl.replace acc trailhead
               (match Hashtbl.find_opt acc trailhead with
               | Some score -> score + 1
               | None -> 1)
           in
           acc)
         (Hashtbl.create 10)
    |> Hashtbl.to_seq

  let part2 =
    trailheads_ratings |> List.of_seq |> List.map snd |> List.fold_left ( + ) 0
end

module TestCase = Day10 (struct
  let input = test_case
end)

module DayInput = Day10 (struct
  let input = read_day_lines 10
end)

let () = printf "Test 1: %d%!\n" TestCase.part1
let () = printf "Part 1: %d%!\n" DayInput.part1
let () = printf "Test 2: %d%!\n" TestCase.part2
let () = printf "Part 2: %d%!\n" DayInput.part2
