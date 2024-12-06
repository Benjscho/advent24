(*
module IntSet = Set.Make(struct type t = int let compare = compare end)
module IntMap = Map.Make(struct type t = int let compare = compare end)
*)

open Advent24.Parsing;;
(**** PART A ****)
(*
Represent as a graph that can be topographically sorted.

My initial thought was to represent as a graph that can be topographically 
sorted, and then test the inputs against that.

However, it turns out there were potential cycles in the graph, so the only
way to do it is to check each line against the rules.

For each rule we check if the numbers are present in the line. If both 
are present, the rule is checked, otherwise it's ignored. 

*)


let read_file filename =
  In_channel.with_open_text filename In_channel.input_all

let file_contents = read_file "./inputs/day-5.txt"

module IntSet = Set.Make(Int)
module IntMap = Map.Make(Int)

(* Store ordering rules as a map of int to set of ints that should come after it *)
type _ordering_rules = IntSet.t IntMap.t

let debug_print_rules rules =
  Printf.printf "Ordering Rules:\n";
  IntMap.iter (fun before after_set ->
    Printf.printf "%d must come before: " before;
    IntSet.iter (fun after ->
      Printf.printf "%d " after
    ) after_set;
    Printf.printf "\n"
  ) rules;
  Printf.printf "\n"

(* Parse input string into ordering rules *)
let parse_ordering_rules lines =
  List.fold_left (fun rules line ->
    match String.split_on_char '|' line with
    | [before; after] ->
        let before = int_of_string before in
        let after = int_of_string after in
        IntMap.update before
          (function
            | None -> Some (IntSet.singleton after)
            | Some s -> Some (IntSet.add after s))
          rules
    | _ -> rules  (* Ignore invalid lines *)
  ) IntMap.empty lines

let is_valid_line rules line =
  let nodes = List.map int_of_string (String.split_on_char ',' line) in
  Printf.printf "Checking line: %s\n" line;
  
  let rec check_order a b nodes =
    match nodes with
    | [] -> false
    | x :: xs -> 
        if x = a then true
        else if x = b then false
        else check_order a b xs
  in

  let is_valid =
    IntMap.for_all (fun before after_set ->
      IntSet.for_all (fun after ->
        if List.mem before nodes && List.mem after nodes then
          let order_correct = check_order before after nodes in
          Printf.printf "  %d should come before %d: %b\n" before after order_correct;
          order_correct
        else
          true
      ) after_set
    ) rules
  in

  Printf.printf "Line %s is %s\n\n" line (if is_valid then "valid" else "invalid");
  is_valid

(* Check if a line is valid and return the middle node if valid *)
let check_line_and_get_middle rules line =
  let nodes = List.map int_of_string (String.split_on_char ',' line) in
  if is_valid_line rules line then
    let middle_index = (List.length nodes - 1) / 2 in
    Some (List.nth nodes middle_index)
  else
    None

(* Main function *)
let process_input input =
  let (rule_lines, data_lines) = parse_until_empty_line input in
  let rules = parse_ordering_rules (remove_empty_lines rule_lines) in
  let () = debug_print_rules rules in
  let data_input = remove_empty_lines data_lines in
  let () = print_endline ([%show: string list] data_input) in
  
  let sum = List.fold_left (fun sum line -> 
    match check_line_and_get_middle rules line with 
    | Some middle -> sum + middle 
    | None -> sum
  ) 0 data_input
  in 
  sum

let () = 
  let sum = process_input file_contents
  in 
  Printf.printf "Part a: %d\n" sum



(**** PART B ****)

(**
To reorder a line:
- Take the nodes that are present in that line
- Assemble a graph from them using the rules, and only the nodes in the line
- Topologically sort them, and use the result
**)

let reorder_line rules line =
  (* Convert line to list of integers *)
  let nodes = List.map int_of_string (String.split_on_char ',' line) in
  let node_set = IntSet.of_list nodes in
  
  (* Create a graph for nodes in the line, using only relevant rules *)
  let graph = IntMap.filter_map (fun k v -> 
    if IntSet.mem k node_set then
      let relevant_v = IntSet.filter (fun x -> IntSet.mem x node_set) v in
      if IntSet.is_empty relevant_v then None else Some relevant_v
    else 
      None
  ) rules in
  
  (* Topological sort *)
  let rec dfs visited sorted node =
    if IntSet.mem node visited then (visited, sorted)
    else
      let visited = IntSet.add node visited in
      let neighbors = IntMap.find_opt node graph |> Option.value ~default:IntSet.empty in
      let visited, sorted = 
        IntSet.fold (fun neighbor (v, s) -> dfs v s neighbor) neighbors (visited, sorted)
      in
      (visited, node :: sorted)
  in
  
  let topological_sort () =
    let (_, sorted) = 
      IntSet.fold (fun node (visited, sorted) -> 
        if IntSet.mem node visited then (visited, sorted)
        else dfs visited sorted node
      ) node_set (IntSet.empty, [])
    in
    sorted
  in
  
  let sorted_nodes = topological_sort () in
  String.concat "," (List.map string_of_int sorted_nodes)

let process_invalid_line rules line =
  Printf.printf "Invalid line: %s\n" line;
  try
    let reordered = reorder_line rules line in
    Printf.printf "Reordered line: %s\n" reordered;
    if is_valid_line rules reordered then
      Printf.printf "Reordered line is valid\n\n"
    else
      Printf.printf "Warning: Reordered line is still invalid\n\n";
    reordered
  with
  | Failure msg -> 
      Printf.printf "Error: %s\n" msg;
      Printf.printf "Unable to reorder line. Returning original line.\n\n";
      line

(* Modified main function *)
let process_input input =
  let (rule_lines, data_lines) = parse_until_empty_line input in
  let rules = parse_ordering_rules rule_lines in
  
  debug_print_rules rules;
  
  let sum = List.fold_left (fun sum line -> 
    let trimmed_line = String.trim line in
    if is_valid_line rules trimmed_line then
      sum
    else
      let reordered = process_invalid_line rules trimmed_line in
      let nodes = List.map int_of_string (String.split_on_char ',' reordered) in
      let middle_index = (List.length nodes - 1) / 2 in
      sum + List.nth nodes middle_index
  ) 0 (remove_empty_lines data_lines)
  in 
  sum

let () = 
  let sum = process_input file_contents
  in 
  Printf.printf "Part b: %d\n" sum
