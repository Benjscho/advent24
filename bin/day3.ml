
let read_file filename =
  In_channel.with_open_text filename In_channel.input_all

let file_contents = read_file "./inputs/day-3.txt"

let find_mul_exprs str = 
  let regex = Str.regexp "mul(\\([0-9]+\\),\\([0-9]+\\))" in
  let rec find_all start acc =
    try
      let _ = Str.search_forward regex str start in
      let num1 = int_of_string (Str.matched_group 1 str) in
      let num2 = int_of_string (Str.matched_group 2 str) in
      find_all (Str.match_end()) ((num1, num2) :: acc)
    with Not_found ->
      List.rev acc
  in
  find_all 0 []

let pairs = find_mul_exprs file_contents

(*
let () = print_endline ([%show: (int * int) list ] pairs)
*)

let res = List.fold_left (fun acc (a, b) -> (a * b) + acc) 0 pairs

let () = 
  Printf.printf "Part a: %d\n" res;;


let find_do_mul_exprs str = 
  let regex = Str.regexp "\\(do\\|don't\\)()\\|mul(\\([0-9]+\\),\\([0-9]+\\))" in
  let rec process start acc enabled = 
    try
      let _ = Str.search_forward regex str start in
      let matched = Str.matched_string str in 
(*
      let () = Printf.printf "matched: %s\n" matched in
*)

      match matched with 
      | "do()" -> 
        process (Str.match_end()) acc true
      | "don't()" -> 
        process (Str.match_end()) acc false 
      | _ -> 
        let num1 = int_of_string (Str.matched_group 2 str) in
        let num2 = int_of_string (Str.matched_group 3 str) in
        let new_acc = if enabled then ((num1, num2) :: acc) else acc in
        process (Str.match_end()) new_acc enabled
    with Not_found ->
      List.rev acc
  in
  process 0 [] true

let pairs = find_do_mul_exprs file_contents

(*
let () = print_endline ([%show: (int * int) list ] pairs)
*)

let res = List.fold_left (fun acc (a, b) -> (a * b) + acc) 0 pairs

let () = 
  Printf.printf "Part b: %d\n" res;;
