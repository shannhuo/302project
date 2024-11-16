exception Msg of string
let invalidInput = Msg("invalidInput (use variable indices as integers)")

type expression =
  | Var of int
  | Not of expression
  | And of expression * expression
  | Or of expression * expression


(* PRINTING *)
let rec printExpression (e: expression): string = match e with (* recursive *)
  | Var(u) -> string_of_int u
  | Not(u) -> "¬" ^ printExpression u
  | And(u, v) -> "(" ^ printExpression u ^ " ∧ " ^ printExpression v ^ ")"
  | Or(u, v) -> "(" ^ printExpression u ^ " ∨ " ^ printExpression v ^ ")"
let printExpression' (e: expression) : string = (* with continuations *)
  let rec trPrintExpression (e: expression) (acc: string -> string): string = match e with
    |Var(u) -> acc (string_of_int u)
    |Not(u) -> trPrintExpression u (fun r -> acc ("¬" ^ r))
    |And(u, v) -> trPrintExpression u (fun r1  -> trPrintExpression v (fun r2 -> acc ("(" ^ r1 ^ " ∧ " ^ r2 ^ ")")))
    |Or(u, v) -> trPrintExpression u (fun r1  -> trPrintExpression v (fun r2 -> acc ("(" ^ r1 ^ " ∨ " ^ r2 ^ ")"))) in
  trPrintExpression e (fun r -> r)


(* INPUT COLLECTION *)
let inputList (e: expression): int list = (* recursive *)
  let rec matchInputs e acc = match e with
    | Var u -> if List.mem u acc then acc else u :: acc
    | Not u -> matchInputs u acc
    | And(u, v) | Or(u, v) -> matchInputs u (matchInputs v acc)
  in
  List.sort (fun a b -> if a<b then 0 else 1) (matchInputs e [])
let trInputList (e: expression): int list = (* with continuations *)
  let rec matchInputs e acc sc = match e with
    | Var u -> if List.mem u acc then sc acc else sc (u :: acc)
    | Not u -> matchInputs u acc sc
    | And(u, v) | Or(u, v) -> matchInputs u acc (fun r -> sc (matchInputs v r sc))
  in
  List.sort (fun a b -> if a<b then 0 else 1) (matchInputs e [] (fun r -> r)) (* sort by increasing order *)


(* COMBINATION GENERATION *)
let rec generateCombinations n : (bool list) list = (* will generate in binary decreasing order *)
  if n = 0 then [[]]
  else
    let sub_combinations = generateCombinations (n - 1) in
      List.map (fun comb -> true :: comb) sub_combinations @
      List.map (fun comb -> false :: comb) sub_combinations

(* EVALUATING WITH MEMOIZATION *)
let memoEvaluateExpression = (* partial evaluation *)
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
        (Hashtbl.add store (e, values) result;
        result)
      in eval

(* TRUTH TABLE GENERATION *)
let truthTable (e : expression) : (bool list * bool) list =
  let vars = inputList e in
  let combinations = generateCombinations (List.length vars) in
  List.map (fun comb ->
      let values = List.combine vars comb in
      (comb, memoEvaluateExpression e values)
    ) combinations

(* format: list of ([var1=bool1, ..., varn=booln], eval)  *)   

(* PRINTING THE TRUTH TABLE *)
let printTruthTable (e : expression) =
  let vars = inputList e in
  let table = truthTable e in
  Printf.printf "Truth table for %s:\n" (printExpression e);
  Printf.printf "%12s | Result\n" (String.concat "    " (List.map string_of_int vars));
  List.iter (fun (comb, result) ->
      Printf.printf "%12s | %B\n" (String.concat " " (List.map string_of_bool comb)) result
    ) table

(* SAT STUFF *)
let makeTrueList n = 
  let rec makeTrueList' n acc =
    if n = 0 then acc else makeTrueList' (n-1) (true::acc) in
  makeTrueList' n []

let alwaysTrue (e: expression) : bool = 
  let table = truthTable e in
  let resultants = List.map (fun (a, b) -> b) table in
  resultants = makeTrueList (List.length table)
let existsSolution (e: expression) : bool = 
  let table = truthTable e in
  let resultants = List.map (fun (a, b) -> b) table in
  List.mem true resultants

let satSolverImplies evaluator (e1 : expression) (e2 : expression) : bool = 
  let e = Or(Not(e1), e2) in
  alwaysTrue e
let satSolverImpliedBy evaluator (e1 : expression) (e2: expression) : bool = 
  let e = Or(Not(e2), e1) in
  alwaysTrue e
let satSolverIff (e1 : expression) (e2: expression) evaluator : bool = 
  let e = Or(And(e1, e2), And(Not(e1), Not(e2))) in
  alwaysTrue e

  
(*SOLUTION SET*)
let findSolutions (e: expression) =
  let vars = inputList e in
  let combinations = generateCombinations (List.length vars) in
  let combPairs = List.map (fun r -> List.combine vars r) combinations in
  let rec findComb comb acc = match comb with
  |[] -> acc
  |h::t -> if memoEvaluateExpression e h then findComb t (h::acc) else findComb t acc 
in findComb combPairs []

(* TESTING *)
let test1 = And(Not(Or(Var(1), And(Var(1), Var(2)))), Or(And(Var(3), Var(2)), And(Not(Var(1)), Var(3))))
let test2 = And(Not(Var(3)), Var(2))
let test3 = And(And(Or(Or(Not(Var(3)), Var(10)), Or(Not(Var(3)), And(Var(2), Var(9)))), 
Or(Var(1), Or(Or(Var(6), Var(5)), Var(2)))), Or(Or(Var(5), Or(Or(Var(8), Var(3)), Not(Var(5)))), Not(Var(10))))

let () =
  printExpression test1 |> Printf.printf "Expression 1: %s\n";
  printExpression test2 |> Printf.printf "Expression 2: %s\n";
  printTruthTable test1;
  printTruthTable test2;;

findSolutions test2;;