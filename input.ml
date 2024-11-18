open Lwt
open Cohttp
open Cohttp_lwt_unix

let today =
  try int_of_string Sys.argv.(1)
  with _ -> (Unix.localtime (Unix.time ())).tm_mday

let cookie =
  let ch = open_in ".cookie" in
  let len = in_channel_length ch in
  let content = really_input_string ch len in
  close_in ch;
  content |> String.trim

let input_url day =
  Printf.sprintf "https://adventofcode.com/2023/day/%d/input" day
  |> Uri.of_string

let save_input day body =
  let path = Printf.sprintf "./input/%d.txt" day in
  Lwt_io.with_file ~mode:Lwt_io.Output path (fun fp -> Lwt_io.write fp body)

let download_input day =
  let headers =
    Header.init () |> fun h ->
    Header.add h "Cookie" cookie |> fun h ->
    Header.add h "User-Agent"
      "Input download script by https://github.com/veeenu"
  in
  Client.get ~headers (input_url day) >>= fun (_resp, body) ->
  Cohttp_lwt.Body.to_string body >>= fun body -> save_input day body

let () = download_input today |> Lwt_main.run
