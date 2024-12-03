let read_day_lines day: string list =
  let fp = Printf.sprintf "_input/day%02d.txt" day |> open_in in
  let lines = In_channel.input_lines fp in
  close_in fp; lines

let split_lines string =
  string |> String.trim |> String.split_on_char '\n'
