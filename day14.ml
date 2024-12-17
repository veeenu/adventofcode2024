open Common
open Printf
open Scanf

let test_case =
  {|
p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3
|}
  |> String.trim |> String.split_on_char '\n'

let day_input = read_day_lines 14

let parse line =
  sscanf line "p=%d,%d v=%d,%d" (fun px py vx vy -> (px, py, vx, vy))

let simulate_count mx my count (px, py, vx, vy) =
  let rec simulate px py count =
    if count > 0 then
      simulate (modulo (px + vx) mx) (modulo (py + vy) my) (count - 1)
    else (px, py)
  in
  simulate px py count

let simulate_all_count robots mx my count =
  robots
  |> List.map (simulate_count mx my count)
  |> List.fold_left
       (fun acc p ->
         let () =
           Hashtbl.find_opt acc p |> function
           | None -> Hashtbl.replace acc p 1
           | Some count -> Hashtbl.replace acc p (count + 1)
         in
         acc)
       (Hashtbl.create 10)

let safety_factor m mx my =
  let mx = mx / 2 in
  let my = my / 2 in
  let () = printf "%d %d\n" mx my in
  let nw, ne, sw, se =
    m |> Hashtbl.to_seq
    |> Seq.filter (fun ((x, y), _) -> x != mx && y != my)
    |> Seq.fold_left
         (fun (nw, ne, sw, se) ((x, y), count) ->
           if x < mx && y < my then (nw + count, ne, sw, se)
           else if x > mx && y < my then (nw, ne + count, sw, se)
           else if x < mx && y > my then (nw, ne, sw + count, se)
           else (nw, ne, sw, se + count))
         (0, 0, 0, 0)
  in
  let () = printf "%d %d %d %d\n" nw ne sw se in
  nw * ne * sw * se

let part1 mx my robots =
  let sim = simulate_all_count robots mx my 100 in
  let () = Hashtbl.iter (fun (x, y) v -> printf "(%d,%d) -> %d\n" x y v) sim in
  safety_factor sim mx my

let () = test_case |> List.map parse |> part1 11 7 |> printf "Test 1: %d\n"
let () = day_input |> List.map parse |> part1 101 103 |> printf "Part 1: %d\n"

let print_sim i robots =
  let m = simulate_all_count robots 101 103 i in
  range_up 0 103
  |> List.iter (fun y ->
         let () =
           range_up 0 101
           |> List.map (fun x ->
                  if Hashtbl.mem m (x, y) then "\x1b[31m#\x1b[0m"
                  else "\x1b[30m.\x1b[0m")
           |> List.iter (printf "%s")
         in
         printf "\n")

let robots = day_input |> List.map parse

let check_candidate i =
  let m = simulate_all_count robots 101 103 i in
  let has_ten_consecutive_robots row =
    range_up 0 91
    |> List.exists (fun col ->
           List.for_all (fun d -> Hashtbl.mem m (col + d, row)) (range_up 0 10))
  in
  range_up 0 103 |> List.exists has_ten_consecutive_robots

let () =
  range_up 0 10000
  |> List.iter (fun i ->
         if check_candidate i then
           let () = printf "\nFOUND: %d%!\n\n\n" i in
           print_sim i robots
         else printf "NOT FOUND: %d%!\r" i)
