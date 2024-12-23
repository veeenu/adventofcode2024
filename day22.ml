open Common
open Printf

let test_case = {|
1
10
100
2024
|} |> String.trim |> String.split_on_char '\n'

let test_case2 = {|
1
2
3
2024
|} |> String.trim |> String.split_on_char '\n'

let day_input = read_day_lines 22
let parse = List.map int_of_string

let evolve secret =
  let step1 = Int.shift_left secret 6 lxor secret land 16777215 in
  let step2 = Int.shift_right step1 5 lxor step1 land 16777215 in
  let step3 = Int.shift_left step2 11 lxor step2 land 16777215 in
  step3

let rec evolve_times count secret =
  if count = 0 then secret else evolve_times (count - 1) (evolve secret)

let part1 input =
  parse input |> List.map (evolve_times 2000) |> List.fold_left ( + ) 0

let prices count secret =
  let rec aux count secret =
    if count = 0 then [] else secret :: aux (count - 1) (evolve secret)
  in
  aux count secret |> List.rev |> List.map (fun i -> i mod 10)

let rec indicators = function
  | lag4 :: lag3 :: lag2 :: lag1 :: price :: xs ->
      ((lag3 - lag4, lag2 - lag3, lag1 - lag2, price - lag1), price)
      :: indicators (lag3 :: lag2 :: lag1 :: price :: xs)
  | _ -> []

module Indicator = struct
  type t = int * int * int * int

  let compare = compare
end

module Indicators = Map.Make (Indicator)
module IndicatorsSet = Set.Make (Indicator)

let indicators_map prices =
  prices |> indicators
  |> List.fold_left
       (fun acc (k, v) ->
         Indicators.update k (function None -> Some v | c -> c) acc)
       Indicators.empty

let find_best_indicator tickers_prices =
  let tickers_indicators = tickers_prices |> List.map indicators_map in
  let key_set =
    tickers_indicators
    |> List.fold_left
         (fun acc indicators ->
           Indicators.fold
             (fun k _ acc -> IndicatorsSet.add k acc)
             indicators acc)
         IndicatorsSet.empty
  in
  let prices_taken =
    key_set |> IndicatorsSet.to_list
    |> List.map (fun indicator ->
           ( indicator,
             tickers_indicators
             |> List.filter_map (fun ticker_indicators ->
                    Indicators.find_opt indicator ticker_indicators) ))
  in
  let total_prices =
    prices_taken
    |> List.map (fun (indicator, prices) ->
           (indicator, prices |> List.fold_left ( + ) 0))
  in
  let best_indicator =
    total_prices
    |> List.fold_left
         (fun (acck, accv) (k, v) -> if v > accv then (k, v) else (acck, accv))
         ((0, 0, 0, 0), 0)
  in
  best_indicator

let part2 input =
  parse input
  |> List.map (prices 2000)
  |> List.map List.rev |> find_best_indicator |> snd

let _ = part1 test_case |> printf "Test 1: %d%!\n"
let _ = part1 day_input |> printf "Part 1: %d%!\n"
let _ = part2 test_case2 |> printf "Test 2: %d%!\n"
let _ = part2 day_input |> printf "Test 2: %d%!\n"
