(**
Boolean Expression Evaluator and SAT Solver
This program includes capabilities such as 
  - Define and manipulate boolean expressions
  - Generate truth tables
  - Evaluate generated boolean expressions 
  - Utilize memoization to optimize processes
  - Solve SAT (satifiability) problems 
  *)
  
(** Exception raised for invalid input such as non-integers *)
exception Msg of string
let invalidInput = Msg("invalidInput (use variable indices as integers)")

(** Represents elements of a boolean expression *)
type expression =
  | Var of int
  | Not of expression
  | And of expression * expression
  | Or of expression * expression

(* HELPER FUNCTION *)
(** Matches a variable's value in the provided list of (int*bool) pairs
  @param u The variable to look up
  @param values A list of (variable, value) pairs
  @return The value of the variable if found
  @raise Msg If variable not found in the list
*)
let matchValue u values = 
  try List.assoc u values with Not_found -> raise invalidInput

(* PRINTING *)
(**
  Converts boolean expression into a human-readable string
  @param e The expression to convert
  @return A string representation of the expression
*)

let rec printExpression (e expression) string = match e with (* recursive *)
  | Var(u) -> string_of_int u
  | Not(u) -> "¬" ^ printExpression u
  | And(u, v) -> "(" ^ printExpression u ^ " ∧ " ^ printExpression v ^ ")"
  | Or(u, v) -> "(" ^ printExpression u ^ " ∨ " ^ printExpression v ^ ")"
let printExpression' (e expression)  string = (* with continuations *)
  let rec trPrintExpression (e expression) (acc string -> string) string = match e with
    |Var(u) -> acc (string_of_int u)
    |Not(u) -> trPrintExpression u (fun r -> acc ("¬" ^ r))
    |And(u, v) -> trPrintExpression u (fun r1  -> trPrintExpression v (fun r2 -> acc ("(" ^ r1 ^ " ∧ " ^ r2 ^ ")")))
    |Or(u, v) -> trPrintExpression u (fun r1  -> trPrintExpression v (fun r2 -> acc ("(" ^ r1 ^ " ∨ " ^ r2 ^ ")"))) in
  trPrintExpression e (fun r -> r)


(* INPUT COLLECTION *)
(**
  Collects list of unique variable indices used in a boolean expression (recursive and tail-recursive versions)
  @param e The expression to analyze
  @return A sorted list of unique variable indices
*)
let inputList (e expression) int list = (* recursive *)
  let rec matchInputs e acc = match e with
    | Var u -> if List.mem u acc then acc else u  acc
    | Not u -> matchInputs u acc
    | And(u, v) | Or(u, v) -> matchInputs u (matchInputs v acc)
  in
  List.sort (fun a b -> if a<b then 0 else 1) (matchInputs e [])
let trInputList (e expression) int list = (* with continuations *)
  let rec matchInputs e acc sc = match e with
    | Var u -> if List.mem u acc then sc acc else sc (u  acc)
    | Not u -> matchInputs u acc sc
    | And(u, v) | Or(u, v) -> matchInputs u acc (fun r -> sc (matchInputs v r sc))
  in
  List.sort (fun a b -> if a<b then 0 else 1) (matchInputs e [] (fun r -> r)) (* sort by increasing order *)


(* COMBINATION GENERATION *)
(** Generates all possible combinations of boolean values for a given number of variables.
    @param n The number of variables.
    @return A list of boolean combinations.
*)
let rec generateCombinations n  (bool list) list = (* will generate in binary decreasing order *)
  if n = 0 then [[]]
  else
    let sub_combinations = generateCombinations (n - 1) in
      List.map (fun comb -> true  comb) sub_combinations @
      List.map (fun comb -> false  comb) sub_combinations
      
(* EVALUATION FUNCTIONS *)
(** EVALUATING WITH MEMOIZATION 
  Evaluates a boolean expression with memoization to optimize repeated evaluations
    @param e The expression to evaluate.
    @param values A list of (variable, value) pairs.
    @return The result of evaluating the expression.
*)
let memoEvaluateExpression = (* partial evaluation *)
  let store  (expression * (int * bool) list, bool) Hashtbl.t = Hashtbl.create 1000 in

  let rec eval e values =
    (* Check memoization store *)
    match Hashtbl.find_opt store (e, values) with
    | Some result -> result
    | None ->
        (* Evaluate based on expression type *)
        let result = match e with
          | Var u -> matchValue u values
          | Not u -> not (eval u values)
          | And(u, v) -> (eval u values) && (eval v values)
          | Or(u, v) -> (eval u values) || (eval v values)
        in
        (* Memoize and return result *)
        (Hashtbl.add store (e, values) result;
        result)
      in eval

(** EVALUATING REGULARLY 
  Evaluates a boolean expression using a standard recursive approach.

    @param e The boolean expression to evaluate.
    @param values A list of `(variable, value)` pairs representing the variable assignments.
    @return The result of evaluating the expression with the given variable assignments.
*)
let evaluateExpression =
  let rec eval e values = match e with 
    |Var(u) -> matchValue u values
    |Not(u) -> not (eval u values)
    |And(u, v) -> let u' = (eval u values) and v' = (eval v values) in (u' && v')
    |Or(u, v) -> let u' = (eval u values) and v' = (eval v values) in (u' || v') in
  eval

(** EVALUATING TAIL RECURSIVELY 
 Evaluates a boolean expression using a tail-recursive approach with continuations.

    @param e The boolean expression to evaluate.
    @param values A list of `(variable, value)` pairs representing the variable assignments.
    @return The result of evaluating the expression with the given variable assignments.
*)
let evaluateExpression' =
    let rec trEval (acc bool -> bool) e values = match e with 
      |Var(u) -> acc (matchValue u values)
      |Not(u) -> trEval (fun r -> acc (not r)) u values
      |And(u, v) -> trEval (fun r1 -> trEval (fun r2 -> acc (r1 && r2)) v values) u values
      |Or(u, v) -> trEval (fun r1 -> trEval (fun r2 -> acc (r1 || r2)) v values) u values in
    trEval (fun r -> r)

(* TRUTH TABLE GENERATION *)
(**
   Constructs a truth table for the given expression.
    
    @param evaluator The evaluation function to use.
    @param e The boolean expression.
    @return A list of pairs, where each pair consists of:
            - A boolean combination for the variables.
            - The evaluation result for the expression.
*)
let truthTable evaluator (e  expression)  (bool list * bool) list =
  let vars = inputList e in
  let combinations = generateCombinations (List.length vars) in
  List.map (fun comb ->
      let values = List.combine vars comb in
      (comb, evaluator e values)
    ) combinations

(* format list of ([var1=bool1, ..., varn=booln], eval)  *)   

(* PRINTING THE TRUTH TABLE *)
(**
  Prints the truth table for a given expression.
    @param evaluator The evaluation function to use.
    @param e The boolean expression.
*)

let printTruthTable evaluator (e  expression) =
  let vars = inputList e in
  let table = truthTable evaluator e in
  Printf.printf "Truth table for %s\n" (printExpression e);
  Printf.printf "%12s | Result\n" (String.concat "    " (List.map string_of_int vars));
  List.iter (fun (comb, result) ->
      Printf.printf "%12s | %B\n" (String.concat " " (List.map string_of_bool comb)) result
    ) table

(* SAT FUNCTIONS *)
(**
  Generates a list containing 'n' boolean 'true' values.
    @param n The number of 'true' values to generate.
    @return A list of 'true' values of length 'n'.
*)
let makeTrueList n = 
  let rec makeTrueList' n acc =
    if n = 0 then acc else makeTrueList' (n-1) (trueacc) in
  makeTrueList' n []
  
(** 
    Determines if a boolean expression is always true for all variable combinations.
    
    @param evaluator The evaluation function to use.
    @param e The boolean expression.
    @return `true` if the expression is always true, otherwise `false`.
*)
let alwaysTrue evaluator (e expression)  bool = 
  let table = truthTable evaluator e in
  let resultants = List.map (fun (a, b) -> b) table in
  resultants = makeTrueList (List.length table)
  
(** 
  Determines if there exists at least one solution for which the boolean expression is true.
  
  @param evaluator The evaluation function to use.
  @param e The boolean expression.
  @return `true` if a solution exists, otherwise `false`.
*)
let existsSolution evaluator (e expression)  bool = 
  let table = truthTable evaluator e in
  let resultants = List.map (fun (a, b) -> b) table in
  List.mem true resultants
  
(** 
  Checks if one boolean expression implies another using SAT solving.
  
  @param evaluator The evaluation function to use.
  @param e1 The first boolean expression.
  @param e2 The second boolean expression.
  @return `true` if `e1` implies `e2`, otherwise `false`.
*)
let satSolverImplies evaluator (e1  expression) (e2  expression)  bool = 
  let e = Or(Not(e1), e2) in
  alwaysTrue evaluator e

(** 
    Checks if one boolean expression is implied by another using SAT solving.
    
    @param evaluator The evaluation function to use.
    @param e1 The first boolean expression.
    @param e2 The second boolean expression.
    @return `true` if `e2` implies `e1`, otherwise `false`.
*)
let satSolverImpliedBy evaluator (e1  expression) (e2 expression)  bool = 
  let e = Or(Not(e2), e1) in
  alwaysTrue evaluator e
  
(** 
    Checks if two boolean expressions are logically equivalent using SAT solving.
    
    @param evaluator The evaluation function to use.
    @param e1 The first boolean expression.
    @param e2 The second boolean expression.
    @return `true` if `e1` is equivalent to `e2`, otherwise `false`.
*)
let satSolverIff (e1  expression) (e2 expression) evaluator  bool = 
  let e = Or(And(e1, e2), And(Not(e1), Not(e2))) in
  alwaysTrue evaluator e

  
(**SOLUTION SET FUNCTIONS
    Finds all solutions (variable assignments) for which a boolean expression evaluates to `true`.
    
    @param evaluator The evaluation function to use.
    @param e The boolean expression.
    @return A list of variable assignments (as lists of `(variable, value)` pairs) that satisfy the expression.
*)
let findSolutions evaluator (e expression) =
  let vars = inputList e in
  let combinations = generateCombinations (List.length vars) in
  let combPairs = List.map (fun r -> List.combine vars r) combinations in
  let rec findComb comb acc = match comb with
  |[] -> acc
  |ht -> if evaluator e h then findComb t (hacc) else findComb t acc 
in findComb combPairs []

(* TESTING *)
let test1 = And(Not(Or(Var(1), And(Var(1), Var(2)))), Or(And(Var(3), Var(2)), And(Not(Var(1)), Var(3))))
let test2 = And(Not(Var(3)), Var(2))
let test3 = And(And(Or(Or(Not(Var(3)), Var(10)), Or(Not(Var(3)), And(Var(2), Var(9)))), 
Or(Var(1), Or(Or(Var(6), Var(5)), Var(2)))), Or(Or(Var(5), Or(Or(Var(8), Var(3)), Not(Var(5)))), Not(Var(10))))

let () =
  printExpression test1 |> Printf.printf "Expression 1 %s\n";
  printExpression test2 |> Printf.printf "Expression 2 %s\n";
  printTruthTable evaluateExpression test1;
  printTruthTable evaluateExpression test2;;

findSolutions evaluateExpression' test2;;
printTruthTable evaluateExpression' test2;;
printTruthTable evaluateExpression test2;;
