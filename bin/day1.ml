(***** PART A *****)

(*
Problem 1:
- Take two lists in as new line delimited tuples 
- Sort both lists 
- Iterate through lists in increasing order pairing up 
- Find the difference of those
- sum up the differences
*)


let parse_input str =
  let lines = String.split_on_char '\n' str in 
  let non_empty_lines = List.filter (fun line -> String.trim line <> "") lines in
    let parse_line str = 
          Scanf.sscanf str "%d %d" (fun a b -> (a, b))
    in 
    let pairs = List.map parse_line non_empty_lines in
    List.split pairs


let read_file filename =
  In_channel.with_open_text filename In_channel.input_all

let file_contents = read_file "./inputs/day-1.txt"

let (list1, list2) = parse_input file_contents

(*
let () =
  Printf.printf "First list: %s\n" (String.concat "; " (List.map string_of_int list1));
  Printf.printf "Second list: %s\n" (String.concat "; " (List.map string_of_int list2))
*)

let list1_sorted = List.sort compare list1
let list2_sorted = List.sort compare list2

(*
let () =
  Printf.printf "Sorted First list: %s\n" (String.concat "; " (List.map string_of_int list1_sorted));
  Printf.printf "Sorted Second list: %s\n" (String.concat "; " (List.map string_of_int list2_sorted))
*)

let sum_differences l1 l2 = 
  let zipped = List.combine l1 l2 in 
  List.map (fun (a, b) -> 
    abs (a - b)
  ) zipped

let diffs = sum_differences list1_sorted list2_sorted

(*
let () =
  Printf.printf "diff list: %s\n" (String.concat "; " (List.map string_of_int diffs));;
*)

let res = List.fold_left (+) 0 diffs

let () = Printf.printf "Part a: %d\n" res;;


(***** PART B *****)

(*
- Make a hashmap of the second list which is defined by number of 
  times a number appears in it 
  - NOTE: this is a super procedural way of doing things, but 
    sometimes ya just gotta define a fuckin variable and mutate it

- Use that table to sum the "similarity" of the left list.

*)

module IntMap = Map.Make(Int)

let frequency_table list =
  List.fold_left (fun acc num -> 
    IntMap.update num (function 
        | None -> Some 1
        | Some count -> Some (count + 1)
    ) acc
  ) IntMap.empty list


let part_b list1 list2 = 
  let map = frequency_table list2 in 
  List.fold_left (fun acc v -> 
    let freq = IntMap.find_opt v map in
    match freq with
    | Some f -> acc + (v * f)
    | _ -> acc
  ) 0 list1

let b_res = part_b list1 list2


let () = Printf.printf "Part b: %d\n" b_res;;
