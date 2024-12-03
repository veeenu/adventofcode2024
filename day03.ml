open Common
open Printf

let input =
  read_day_lines 3
  |> List.map (fun x -> x |> String.to_seq |> List.of_seq)
  |> List.concat

exception ParseFailure
exception Unreachable

type token = Do | Dont | Mul of (int * int)

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
  try
    let xs = parse_prefix [ 'm'; 'u'; 'l'; '(' ] xs in
    let n1, xs = parse_numbers xs in
    let xs = parse_prefix [ ',' ] xs in
    let n2, xs = parse_numbers xs in
    let xs = parse_prefix [ ')' ] xs in
    Some (Mul (n1, n2), xs)
  with ParseFailure -> None

let rec parse1 = function
  | [] -> []
  | input -> (
      match read_mul input with
      | Some (Mul (n1, n2), xs) -> (n1, n2) :: parse1 xs
      | None -> List.tl input |> parse1
      | _ -> raise Unreachable)

let part1 =
  parse1 input |> List.fold_left (fun acc (n1, n2) -> acc + (n1 * n2)) 0

let () = printf "Part 1: %d\n" part1

let read_do xs =
  try Some (Do, parse_prefix [ 'd'; 'o'; '('; ')' ] xs)
  with ParseFailure -> None

let read_dont xs =
  try Some (Dont, parse_prefix [ 'd'; 'o'; 'n'; '\''; 't'; '('; ')' ] xs)
  with ParseFailure -> None

let read_token xs =
  try
    List.find Option.is_some
      (List.map (fun f -> f xs) [ read_do; read_dont; read_mul ])
  with Not_found -> None

let rec parse2 = function
  | [] -> []
  | input -> (
      match read_token input with
      | Some (tok, xs) -> tok :: parse2 xs
      | None -> List.tl input |> parse2)

type scan = Enabled of int | Disabled of int

let disable = function Enabled x -> Disabled x | Disabled x -> Disabled x
let enable = function Enabled x -> Enabled x | Disabled x -> Enabled x

let mul acc n1 n2 =
  match acc with
  | Enabled x -> Enabled (x + (n1 * n2))
  | Disabled x -> Disabled x

let part2 =
  parse2 input
  |> List.fold_left
       (fun acc tok ->
         match tok with
         | Do -> enable acc
         | Dont -> disable acc
         | Mul (n1, n2) -> mul acc n1 n2)
       (Enabled 0)
  |> function
  | Enabled x -> x
  | Disabled x -> x

let () = printf "Part 2: %d\n" part2
