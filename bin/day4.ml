(**** PART A ****)
(*
Wordsearch time! 
Take each row, reverse it 
take each column, reverse it 
take each up diagonal, rev 
take each down diagonal, rev 
Regex to find number of xmas matches in each 
*)

open Advent24.Parsing;;

let read_file filename =
  In_channel.with_open_text filename In_channel.input_all

let file_contents = read_file "./inputs/day-4.txt"

let lines = read_non_empty_lines file_contents 

let input_matrix = List.map (fun s -> List.init (String.length s) (String.get s)) lines

let get_rows matrix = matrix

let get_columns matrix =
  List.init (List.length (List.hd matrix)) (fun i ->
    List.map (fun row -> List.nth row i) matrix
  )

let get_upward_diagonals matrix =
  let height = List.length matrix in
  let width = List.length (List.hd matrix) in
  let diags = ref [] in
  for sum = 0 to height + width - 2 do
    let diag = ref [] in
    for i = 0 to height - 1 do
      let j = sum - i in
      if j >= 0 && j < width then
        diag := (List.nth (List.nth matrix i) j) :: !diag
    done;
    if !diag <> [] then diags := !diag :: !diags
  done;
  List.rev !diags

let get_downward_diagonals matrix =
  let height = List.length matrix in
  let width = List.length (List.hd matrix) in
  let diags = ref [] in
  for diff = -(height - 1) to width - 1 do
    let diag = ref [] in
    for i = 0 to height - 1 do
      let j = i + diff in
      if j >= 0 && j < width then
        diag := (List.nth (List.nth matrix i) j) :: !diag
    done;
    if !diag <> [] then diags := (List.rev !diag) :: !diags
  done;
  !diags

let chars_to_string chars =
  String.init (List.length chars) (fun i -> List.nth chars i)

(* Use the matrix as before *)
let matrix_to_string_arr matrix = 
  let rows = get_rows matrix in
  let columns = get_columns matrix in 
  let up_diag = get_upward_diagonals matrix in
  let down_diag = get_downward_diagonals matrix in

  let all_lists = rows @ columns @ up_diag @ down_diag in 
  List.map chars_to_string all_lists


let count_regex_occurrences regex_string text =
  let regex = Str.regexp regex_string in
  let count = ref 0 in
  let _ = 
    try
      let _ = Str.search_forward regex text 0 in
      incr count;
      while true do
        let _ = Str.search_forward regex text (Str.match_end()) in
        incr count
      done
    with Not_found -> ()
  in
  !count

let xmas = "XMAS"

let reverse_string s =
  String.init (String.length s) (fun i -> s.[String.length s - 1 - i])

let res = List.fold_left (fun acc str -> (count_regex_occurrences xmas str) + (count_regex_occurrences xmas (reverse_string str)) + acc) 0 (matrix_to_string_arr input_matrix)

let () = Printf.printf "Part a: %d\n" res;;

(**** PART B ****)

(*
First thought is to add an index to each letter, and when we match it, add 
the index to a list. We do that for each of the diagonal lists, and only
count the occurrences where they intersect. 

It's pretty messy and requires multiple iterations, but works.

*)

let get_upward_diagonals_with_coords matrix =
  let height = List.length matrix in
  let width = List.length (List.hd matrix) in
  let diags = ref [] in
  for sum = 0 to height + width - 2 do
    let diag = ref [] in
    for i = 0 to height - 1 do
      let j = sum - i in
      if j >= 0 && j < width then
        diag := ((List.nth (List.nth matrix i) j), (j, i)) :: !diag
    done;
    if !diag <> [] then diags := !diag :: !diags
  done;
  List.rev !diags

let get_downward_diagonals_with_coords matrix =
  let height = List.length matrix in
  let width = List.length (List.hd matrix) in
  let diags = ref [] in
  for diff = -(height - 1) to width - 1 do
    let diag = ref [] in
    for i = 0 to height - 1 do
      let j = i + diff in
      if j >= 0 && j < width then
        diag := ((List.nth (List.nth matrix i) j), (j, i)) :: !diag
    done;
    if !diag <> [] then diags := (List.rev !diag) :: !diags
  done;
  !diags

  (* Helper function to print the results *)
let print_diagonals name diagonals =
  Printf.printf "%s:\n" name;
  List.iter (fun diagonal ->
    List.iter (fun (char, (x, y)) ->
      Printf.printf "(%c, (%d, %d)) " char x y
    ) diagonal;
    print_newline ()
  ) diagonals;
  print_newline ()

let () =
  let upward = get_upward_diagonals_with_coords input_matrix in
  let downward = get_downward_diagonals_with_coords input_matrix in
  print_diagonals "Upward Diagonals" upward;
  print_diagonals "Downward Diagonals" downward

let rec search_tuples_for_word acc word tuples = 
  match tuples with
    | [] -> acc
    | (c1, (_, _)) :: (c2, (x, y)) :: (c3, (_, _)) :: xs when
      c1 = word.[0] && c2 = word.[1] && c3 = word.[2] ->
        search_tuples_for_word ((x, y) :: acc) word xs 
    | _ :: xs -> search_tuples_for_word acc word xs

(* Helper function to print results *)
let print_results word results =
  Printf.printf "Occurrences of '%s':\n" word;
  List.iter (fun el ->
    print_endline ([%show: int * int] el)
  ) results;
  print_newline ()

let intersection_of_coordinates list1 list2 =
  let set1 = List.sort_uniq compare list1 in
  let set2 = List.sort_uniq compare list2 in
  
  let rec intersect acc l1 l2 =
    match l1, l2 with
    | [], _ | _, [] -> List.rev acc
    | h1::t1, h2::t2 ->
        if h1 = h2 then intersect (h1::acc) t1 t2
        else if h1 < h2 then intersect acc t1 l2
        else intersect acc l1 t2
  in
  
  intersect [] set1 set2

let crosses =
  let upward = get_upward_diagonals_with_coords input_matrix in
  let downward = get_downward_diagonals_with_coords input_matrix in
  let up_forward_coords = List.fold_left (fun acc row -> (search_tuples_for_word [] "MAS" row) @ acc) [] upward in
  let up_back_coords = List.fold_left (fun acc row -> (search_tuples_for_word [] "MAS" (List.rev row)) @ acc) [] upward in
  let down_forward_coords = List.fold_left (fun acc row -> (search_tuples_for_word [] "MAS" row) @ acc) [] downward in
  let down_back_coords = List.fold_left (fun acc row -> (search_tuples_for_word [] "MAS" (List.rev row)) @ acc) [] downward in
  let () = Printf.printf "up forward: \n" in
  let () = print_results "MAS" up_forward_coords in 
  let () = Printf.printf "up back: \n" in
  let () = print_results "MAS" up_back_coords in 
  let all_up_coords = up_forward_coords @ up_back_coords in 
  let all_down_coords = down_forward_coords @ down_back_coords in 
  intersection_of_coordinates all_up_coords all_down_coords

let res = List.length crosses

let () = Printf.printf "Part b: %d \n" res
