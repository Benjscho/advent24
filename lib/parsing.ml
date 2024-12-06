let read_non_empty_lines str =
    let lines = String.split_on_char '\n' str in
    List.filter (fun line -> String.trim line <> "") lines

let remove_empty_lines lines = 
    List.filter (fun line -> String.trim line <> "") lines

let parse_numbers_from_string str =
  str
  |> String.split_on_char ' '  (* Split the string on spaces *)
  |> List.filter (fun s -> s <> "")  (* Remove empty strings *)
  |> List.map int_of_string  (* Convert each string to a float *)

let parse_until_empty_line input =
  let lines = String.split_on_char '\n' input in
  let rec aux acc = function
    | [] -> List.rev acc, []  (* End of input *)
    | "" :: rest -> List.rev acc, rest  (* Empty line found *)
    | line :: rest -> aux (line :: acc) rest
  in
  aux [] lines

