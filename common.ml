let read_day_lines day : string list =
  let fp = Printf.sprintf "_input/day%02d.txt" day |> open_in in
  let lines = In_channel.input_lines fp in
  close_in fp;
  lines

let split_lines string = string |> String.trim |> String.split_on_char '\n'
let to_grid l = l |> List.map (fun x -> x |> String.to_seq |> List.of_seq)
let range_up start count = List.init count (fun i -> start + i)
let range_dn start count = List.init count (fun i -> start - i)

let inspect fn list =
  List.map
    (fun x ->
      fn x;
      x)
    list

let inspect_one fn el =
  let () = fn el in
  el

module type AocInput = sig
  val input : string list
end

module Grid : sig
  type 'a t

  val of_lines : (char -> 'a) -> string list -> 'a t
  val width : 'a t -> int
  val height : 'a t -> int
  val at : 'a t -> int -> int -> 'a
  val indices : 'a t -> (int * int) list
  val items : 'a t -> (int * int * 'a) list
  val is_in_bounds : 'a t -> int -> int -> bool
end = struct
  type 'a t = 'a list list

  let of_lines mapper lines =
    List.map (fun row -> List.map mapper row) (to_grid lines)

  let width t = List.length (List.nth t 0)
  let height t = List.length t
  let at t x y = List.nth (List.nth t y) x

  let indices t =
    let range_w = range_up 0 (width t) in
    let range_h = range_up 0 (height t) in
    List.map (fun y -> List.map (fun x -> (x, y)) range_w) range_h
    |> List.flatten

  let items t = indices t |> List.map (fun (x, y) -> (x, y, at t x y))
  let is_in_bounds t x y = x >= 0 && y >= 0 && x < width t && y < height t
end
