open Common
open Printf

let test_case = {|2333133121414131402|}

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

let checksum fs =
  let checksum_value idx = function File id -> idx * id | Empty -> 0 in
  Array.mapi checksum_value fs |> Array.fold_left ( + ) 0

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

  let rec swap_loop i n =
    if i < n then
      match fs.(i) with
      | File _ -> swap_loop (i + 1) n
      | Empty -> swap_loop (i + 1) (swap_last_with i n)
    else ()
  in

  let () = swap_loop 0 (Array.length fs - 1) in

  checksum fs

let part2 input =
  let fs = input |> List.mapi (fun id (file, space) -> (id, file, space)) in
  let rfs = List.rev fs in

  let rec take_while pred l =
    match l with
    | [] -> []
    | hd :: tl -> if pred hd then hd :: take_while pred tl else []
  in

  let swap_slots fs id (rid, rfile, _) =
    fs
    |> List.fold_left
         (fun (found, nfs) (vid, vfile, vspace) ->
           if vid = id && not found then
             (true, (rid, rfile, vspace - rfile) :: (vid, vfile, 0) :: nfs)
           else if vid = rid then (found, (rid, 0, rfile + vspace) :: nfs)
           else (found, (vid, vfile, vspace) :: nfs))
         (false, [])
    |> snd |> List.rev
  in

  let find_slot fs rid rfile =
    fs
    |> take_while (fun (id, _, _) -> id != rid)
    |> List.find_opt (fun (_, _, space) -> space >= rfile)
    |> Option.map (fun (id, _, _) -> id)
  in

  let try_swap_slots fs (rid, rfile, rspace) =
    find_slot fs rid rfile |> function
    | Some id -> swap_slots fs id (rid, rfile, rspace)
    | None -> fs
  in

  let rec defrag rev_fs fs =
    match rev_fs with [] -> fs | hd :: tl -> try_swap_slots fs hd |> defrag tl
  in

  defrag rfs fs
  |> List.map (fun (id, file, space) -> expand id (file, space))
  |> List.flatten |> Array.of_list |> checksum

let () = printf "Test 1: %d\n" (parse test_case |> part1)
let () = printf "Part 1: %d\n" (parse (read_day_lines 9 |> List.hd) |> part1)
let () = printf "Test 2: %d\n" (parse test_case |> part2)
let () = printf "Part 2: %d\n" (parse (read_day_lines 9 |> List.hd) |> part2)
