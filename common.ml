let read_day_lines day: string list =
  let path = Printf.sprintf "_input/day%02d.txt" day in
  let fp = open_in path in
  let rec read_lines acc =
    try
      let line = input_line fp in
      read_lines (line :: acc)
    with End_of_file -> List.rev acc
  in
  read_lines []
