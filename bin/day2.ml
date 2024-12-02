(***** PART A *****)

(*
Problem approach:
- Split lines into lists
- For each list, check if it's safe 
- Check if safe, by asserting that the current level change is
  the same as the previous, and that the abs diff is 1 or 2
*)

type level_changing = 
  | Initial
  | Ascending
  | Descending
  | Terminal [@@deriving show]

open Advent24.Parsing;;


let parse_input str = 
  let lines = read_non_empty_lines str in
  List.map parse_numbers_from_string lines 

let read_file filename =
  In_channel.with_open_text filename In_channel.input_all

let file_contents = read_file "./inputs/day-2-test.txt"

let levels = parse_input file_contents

(*
let () = print_endline ([%show: int list list] levels)
*)

let rec is_safe_level_rec level_dir curr remaining =
  match remaining with
  | [] -> true
  | next :: xs -> 
    let (valid_level, next_dir) = match curr - next with 
      | 0 -> (false, Terminal)
      | x when x > 0 && x <= 3 -> (level_dir = Initial || level_dir = Descending, Descending)
      | x when x < 0 && x >= -3 -> (level_dir = Initial || level_dir = Ascending, Ascending)
      | _ -> (false, Terminal)
    in
(*
    let () = 
      Printf.printf "curr: %d, next: %d, direction: %s, next_dir: %s\n"
        curr 
        next
        ([%show: level_changing] level_dir)
        ([%show: level_changing] next_dir)
    in
*)
    valid_level && is_safe_level_rec next_dir next xs

let safe_level level =
  let res = match level with
  | [] -> true 
  | first :: xs -> 
    is_safe_level_rec Initial first xs
  in 
(*
  let () = print_endline ([%show: bool] res) in 
*)
  res


    
(* 
   This is neat, to count the number of safe levels we're just using 
   safe_level as a filter, and getting the length of the list
 *)
let res = List.length (List.filter safe_level levels)

let () = Printf.printf "Part a: %d\n" res;;


(***** PART B *****)

(*
This should be a straightforward modification of the filter of part A. 
We take the part A solution, and allow one skip. This can just be a boolean
as to whether we've skipped yet.

We should also be able to get it to work with part A by making the skip passed 
in from outside.

Oops I'm wrong! We need to add the ability to backtrack and try skipping 
prior levels
*)

let rec is_safe_level_rec_b free_level level_dir curr remaining =
  match remaining with
  | [] -> true
  | next :: xs -> 
    let (valid_level, next_dir) = match curr - next with 
      | 0 -> (false, level_dir)
      | x when x > 0 && x <= 3 -> (level_dir = Initial || level_dir = Descending, Descending)
      | x when x < 0 && x >= -3 -> (level_dir = Initial || level_dir = Ascending, Ascending)
      | _ -> (false, level_dir)
    in
    let () = 
      Printf.printf "curr: %d, next: %d, direction: %s, next_dir: %s, free_level: %b\n"
        curr 
        next
        ([%show: level_changing] level_dir)
        ([%show: level_changing] next_dir)
        free_level
    in
    match valid_level with 
    | false -> free_level && is_safe_level_rec_b false next_dir curr xs
    | true -> is_safe_level_rec_b free_level next_dir next xs

let safe_level_b level =
  let res = match level with
  | [] -> true 
  | first :: xs -> 
    is_safe_level_rec_b true Initial first xs
  in 
  let () = print_endline ([%show: bool] res) in 
  res

let res = List.length (List.filter safe_level_b levels)

let () = Printf.printf "Part b: %d\n" res;;
