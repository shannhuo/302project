exception Msg of string
let invalidInput = Msg("invalidInput (use variable indices as integers)")

type expression =
  | Var of int
  | Not of expression
  | And of expression * expression
  | Or of expression * expression

(* PRINTING *)
let string_of_bool_list lst =
  "[" ^ String.concat "; " (List.map string_of_bool lst) ^ "]"

let string_of_bool_list_list lst_lst =
  "[" ^ String.concat "; " (List.map string_of_bool_list lst_lst) ^ "]"

let rec printExpression (e: expression): string = match e with
  | Var u -> string_of_int u
  | Not u -> "¬" ^ printExpression u
  | And(u, v) -> "(" ^ printExpression u ^ " ∧ " ^ printExpression v ^ ")"
  | Or(u, v) -> "(" ^ printExpression u ^ " ∨ " ^ printExpression v ^ ")"

(* INPUT COLLECTION *)
let inputList (e: expression): int list = 
  let rec matchInputs e acc = match e with
    | Var u -> if List.mem u acc then acc else u :: acc
    | Not u -> matchInputs u acc
    | And(u, v) | Or(u, v) -> matchInputs u (matchInputs v acc)
  in
  matchInputs e []

(* COMBINATION GENERATION *)
let rec generate_combinations n =
  if n = 0 then [[]]
  else
    let sub_combinations = generate_combinations (n - 1) in
    List.concat [
      List.map (fun comb -> true :: comb) sub_combinations;
      List.map (fun comb -> false :: comb) sub_combinations
    ]

let make_value_pairs vars comb =
  List.combine vars comb

(* EVALUATING WITH MEMOIZATION *)
let memoEvaluateExpression (e: expression) =
  let store : (expression * (int * bool) list, bool) Hashtbl.t = Hashtbl.create 1000 in

  let rec eval e values =
    (* Check memoization store *)
    match Hashtbl.find_opt store (e, values) with
    | Some result -> result
    | None ->
        (* Evaluate based on expression type *)
        let result = match e with
          | Var v -> (try List.assoc v values with Not_found -> raise invalidInput)
          | Not u -> not (eval u values)
          | And(u, v) -> (eval u values) && (eval v values)
          | Or(u, v) -> (eval u values) || (eval v values)
        in
        (* Memoize and return result *)
        Hashtbl.add store (e, values) result;
        result
  in
  eval

(* TRUTH TABLE GENERATION *)
let truthTable (e : expression) : (bool list * bool) list =
  let vars = inputList e in
  let combinations = generate_combinations (List.length vars) in
  let eval = memoEvaluateExpression e in
  List.map (fun comb ->
      let values = make_value_pairs vars comb in
      (comb, eval e values)
    ) combinations

(* PRINTING THE TRUTH TABLE *)
let print_truth_table (e : expression) =
  let vars = inputList e in
  let table = truthTable e in
  Printf.printf "Truth table for %s:\n" (printExpression e);
  Printf.printf "%s | Result\n" (String.concat " " (List.map string_of_int vars));
  List.iter (fun (comb, result) ->
      Printf.printf "%s | %b\n" (String.concat " " (List.map string_of_bool comb)) result
    ) table

(* TESTING *)
let test1 = And(Not(Or(Var(1), And(Var(1), Var(2)))), Or(And(Var(3), Var(2)), And(Not(Var(1)), Var(3))))
let test2 = And(Not(Var(3)), Var(2))
let test3 = Or(And(Var(1), Not(Var(2))), And(Var(3), Var(4)))
let () =
  printExpression test1 |> Printf.printf "Expression 1: %s\n";
  printExpression test2 |> Printf.printf "Expression 2: %s\n";
  printExpression test3 |> Printf.printf "Expression 3: %s\n";

  print_truth_table test1;
  print_truth_table test2
  print_truth_table test 3
