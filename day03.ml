open Common

let input =
  read_day_lines 3
  |> List.map (fun x -> x |> String.to_seq |> List.of_seq)
  |> List.concat

exception ParseFailure

let into_string l = String.init (List.length l) (List.nth l)

let rec take_numbers = function
  | x :: xs when x >= '0' && x <= '9' ->
      take_numbers xs |> fun (nums, ys) -> (x :: nums, ys)
  | xs -> ([], xs)

let parse_numbers xs =
  take_numbers xs |> fun (nums, xs) ->
  try (int_of_string (into_string nums), xs)
  with Failure _ -> raise ParseFailure

let rec parse_prefix prefix input =
  match (prefix, input) with
  | p :: ps, x :: xs when p = x -> parse_prefix ps xs
  | _ :: _, _ -> raise ParseFailure
  | [], xs -> xs

let read_mul xs =
  let xs = parse_prefix [ 'm'; 'u'; 'l'; '(' ] xs in
  let n1, xs = parse_numbers xs in
  let xs = parse_prefix [ ',' ] xs in
  let n2, xs = parse_numbers xs in
  let xs = parse_prefix [ ')' ] xs in
  (n1, n2, xs)

let rec parse input =
  match input with
  | [] -> []
  | input -> (
      try
        let n1, n2, xs = read_mul input in
        (n1, n2) :: parse xs
      with ParseFailure -> List.tl input |> parse)

let part1 =
  parse input |> List.fold_left (fun acc (n1, n2) -> acc + (n1 * n2)) 0

let () = Printf.printf "Part 1: %d\n" part1
