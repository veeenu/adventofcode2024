let read_day_lines day : string list =
  let fp = Printf.sprintf "_input/day%02d.txt" day |> open_in in
  let lines = In_channel.input_lines fp in
  close_in fp;
  lines

let split_lines string = string |> String.trim |> String.split_on_char '\n'
let to_grid l = l |> List.map (fun x -> x |> String.to_seq |> List.of_seq)
let range_up start count = List.init count (fun i -> start + i)
let range_dn start count = List.init count (fun i -> start - i)

let rec take_while pred l =
  match l with
  | [] -> []
  | x :: xs -> if pred x then x :: take_while pred xs else []

let rec drop_while pred l =
  match l with [] -> [] | x :: xs -> if pred x then drop_while pred xs else xs

let rec skip n l =
  match l with [] -> [] | _ :: xs -> if n > 0 then skip (n - 1) xs else xs

let inspect fn list =
  List.map
    (fun x ->
      fn x;
      x)
    list

let inspect_one fn el =
  let () = fn el in
  el

let comparexy (x1, y1) (x2, y2) = compare (y1, x1) (y2, x2)

exception Unreachable

module type AocInput = sig
  val input : string list
end

type direction = North | South | East | West

module Direction : sig
  val move : direction -> int * int -> int * int
  val cw: direction -> direction
  val ccw: direction -> direction
  val flip: direction -> direction
end = struct
  let move d (x, y) =
    match d with
    | North -> (x, y - 1)
    | East -> (x + 1, y)
    | South -> (x, y + 1)
    | West -> (x - 1, y)

  let cw = function North -> East | East -> South | South -> West | West -> North
  let ccw = function North -> West | West -> South | South -> East | East -> North
  let flip = function North -> South | East -> West | South -> North | West -> East
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

module Memo : sig
  val empty : unit -> ('a, 'b) Hashtbl.t
  val exists : ('a, 'b) Hashtbl.t -> 'a -> bool
  val memo : ('a, 'b) Hashtbl.t -> 'a -> ('a -> 'b) -> 'b
end = struct
  let empty () = Hashtbl.create 64
  let exists = Hashtbl.mem

  let memo t key fn =
    Hashtbl.find_opt t key |> function
    | Some value -> value
    | None ->
        let value = fn key in
        let () = Hashtbl.replace t key value in
        value
end
