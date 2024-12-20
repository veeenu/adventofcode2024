let read_day_lines day : string list =
  let fp = Printf.sprintf "_input/day%02d.txt" day |> open_in in
  let lines = In_channel.input_lines fp in
  close_in fp;
  lines

let split_lines string = string |> String.trim |> String.split_on_char '\n'
let to_grid l = l |> List.map (fun x -> x |> String.to_seq |> List.of_seq)
let range_up start count = List.init count (fun i -> start + i)
let range_dn start count = List.init count (fun i -> start - i)
let ( >> ) f g x = g (f x)

let modulo x y =
  let result = x mod y in
  if result >= 0 then result else result + y

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

let char_of_direction = function
  | North -> 'N'
  | South -> 'S'
  | East -> 'E'
  | West -> 'W'

module Direction : sig
  val move : direction -> int * int -> int * int
  val cw : direction -> direction
  val ccw : direction -> direction
  val flip : direction -> direction
end = struct
  let move d (x, y) =
    match d with
    | North -> (x, y - 1)
    | East -> (x + 1, y)
    | South -> (x, y + 1)
    | West -> (x - 1, y)

  let cw = function
    | North -> East
    | East -> South
    | South -> West
    | West -> North

  let ccw = function
    | North -> West
    | West -> South
    | South -> East
    | East -> North

  let flip = function
    | North -> South
    | East -> West
    | South -> North
    | West -> East
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
  val neighbors : 'a t -> int -> int -> (int * int) list
  val at_neighbors : 'a t -> int -> int -> 'a list
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

  let neighbors t x y =
    [ North; South; West; East ]
    |> List.map (fun d -> Direction.move d (x, y))
    |> List.filter (fun (x, y) -> is_in_bounds t x y)

  let at_neighbors t x y = neighbors t x y |> List.map (fun (x, y) -> at t x y)
end

module ArrayGrid : sig
  type 'a t

  val make : int -> int -> 'a -> 'a t
  val of_lines : (char -> 'a) -> string list -> 'a t
  val width : 'a t -> int
  val height : 'a t -> int
  val at : 'a t -> int -> int -> 'a
  val set : 'a t -> int -> int -> 'a -> 'a t
  val clone : 'a t -> 'a t
  val print : 'a t -> (int -> int -> 'a -> string) -> unit
  val indices : 'a t -> (int * int) list
  val items : 'a t -> (int * int * 'a) list
  val is_in_bounds : 'a t -> int -> int -> bool
  val neighbors : 'a t -> int -> int -> (int * int) list
  val at_neighbors : 'a t -> int -> int -> 'a list
end = struct
  type 'a t = 'a array array

  let make w h default = Array.make_matrix h w default

  let of_lines mapper lines =
    to_grid lines |> Array.of_list |> Array.map Array.of_list
    |> Array.map (fun row -> Array.map mapper row)

  let width t = Array.length t.(0)
  let height t = Array.length t
  let at t x y = t.(y).(x)

  let set t x y v =
    let _ = t.(y).(x) <- v in
    t

  let clone (t : 'a t) = t |> Array.map (Array.map (fun col -> col))

  let print t string_of =
    Array.iteri
      (fun y row ->
        let _ =
          Array.iteri (fun x col -> Printf.printf "%s" (string_of x y col)) row
        in
        Printf.printf "%!\n")
      t

  let indices t =
    let range_w = range_up 0 (width t) in
    let range_h = range_up 0 (height t) in
    List.map (fun y -> List.map (fun x -> (x, y)) range_w) range_h
    |> List.flatten

  let items t = indices t |> List.map (fun (x, y) -> (x, y, at t x y))
  let is_in_bounds t x y = x >= 0 && y >= 0 && x < width t && y < height t

  let neighbors t x y =
    [ North; South; West; East ]
    |> List.map (fun d -> Direction.move d (x, y))
    |> List.filter (fun (x, y) -> is_in_bounds t x y)

  let at_neighbors t x y = neighbors t x y |> List.map (fun (x, y) -> at t x y)
end

module Memo : sig
  val empty : unit -> ('a, 'b) Hashtbl.t
  val exists : ('a, 'b) Hashtbl.t -> 'a -> bool
  val memo : ('a, 'b) Hashtbl.t -> 'a -> ('a -> 'b) -> 'b
  val make : ('a -> 'b) -> 'a -> 'b
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

  let make fn =
    let memo_tbl = empty () in
    fun key -> memo memo_tbl key fn
end
