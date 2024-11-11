exception Msg of string
let invalidInput = Msg("invalidInput (use \"a\" or \"b\")")

type expression =
  | Var of string
  | Not of expression
  | And of expression * expression
  | Or of expression * expression;;


(*PRINTING*)
let string_of_bool_list lst =
  "[" ^ String.concat "; " (List.map string_of_bool lst) ^ "]"
let string_of_2_bool_list lst =
  "[" ^ String.concat "; " (List.map (fun (a, b) -> ("("^string_of_bool a ^ ", " ^ string_of_bool b^")")) lst) ^ "]"
let string_of_bool_list_list lst_lst =
  "[" ^ String.concat "; " (List.map string_of_bool_list lst_lst) ^ "]"
let string_of_2_bool_list_list lst_lst =
  "[" ^ String.concat "; " (List.map string_of_2_bool_list lst_lst) ^ "]"
let rec printExpression (e: expression): string = match e with
  |Var("a") -> "a"
  |Var("b") -> "b"
  |Not(u) -> "¬" ^ printExpression u
  |And(u, v) -> "(" ^ printExpression u ^ " ∧ " ^ printExpression v ^ ")"
  |Or(u, v) -> "(" ^ printExpression u ^ " ∨ " ^ printExpression v ^ ")"
  |Var(_) -> raise invalidInput
let rec trPrintExpression (e: expression) (acc: string -> string): string = match e with
  |Var("a") -> acc "a"
  |Var("b") -> acc "b"
  |Not(u) -> trPrintExpression u (fun r -> acc ("¬" ^ r))
  |And(u, v) -> trPrintExpression u (fun r1  -> trPrintExpression v (fun r2 -> acc ("(" ^ r1 ^ " ∧ " ^ r2 ^ ")")))
  |Or(u, v) -> trPrintExpression u (fun r1  -> trPrintExpression v (fun r2 -> acc ("(" ^ r1 ^ " ∨ " ^ r2 ^ ")")))
  |Var(_) -> raise invalidInput
let printExpression' (e : expression) : string = trPrintExpression e (fun r -> r)


(*EVALUATING*)
let rec evaluateExpression (e: expression) (i : bool * bool) : bool = let (a, b) = i in
  match e with 
  |Var("a") -> a
  |Var("b") -> b
  |Not(u) -> not (evaluateExpression u i)
  |And(u, v) -> let u' = (evaluateExpression u i) and v' = evaluateExpression v i in (u' && v')
  |Or(u, v) -> let u' = (evaluateExpression u i) and v' = evaluateExpression v i in (u' || v')
  |Var(_) -> raise invalidInput
let rec trEvaluateExpression (e: expression) (i : bool * bool) (acc: bool -> bool): bool = let (a, b) = i in
  match e with 
  |Var("a") -> acc a
  |Var("b") -> acc b
  |Not(u) -> trEvaluateExpression u i (fun r -> acc (not r))
  |And(u, v) -> trEvaluateExpression u i (fun r1 -> trEvaluateExpression v i (fun r2 -> acc (r1 && r2)))
  |Or(u, v) -> trEvaluateExpression u i (fun r1 -> trEvaluateExpression v i (fun r2 -> acc (r1 || r2)))
  |Var(_) -> raise invalidInput
let evaluateExpression' (e: expression) (i : bool * bool) : bool = trEvaluateExpression e i (fun r -> r)
let rec memoEvaluateExpression (e : expression) (i : bool * bool) =
  let store : (expression, bool) Hashtbl.t = Hashtbl.create 1000 in
  let (a, b) = i in
  let rec memoEval f =
    match Hashtbl.find_opt store e with
    | Some result -> result  
    | None ->
        let result = 
          match f with
          | Var("a") -> a
          | Var("b") -> b
          | Not u -> not (memoEval u)
          | And (u, v) -> (memoEval u) && (memoEval v)
          | Or (u, v) -> (memoEval u) || (memoEval v)
          | Var(_) -> raise invalidInput
      in (Hashtbl.add store f result; result)
    in
    let output = memoEval e in
    (Hashtbl.iter (fun x y -> Printf.printf "%s -> %s\n" (printExpression x) (string_of_bool y)) store; output)


(*SAT SECTION*)
let truthTable2D (e : expression) (evaluator : expression -> (bool * bool) -> bool): ((bool list) list) = 
  let a00 = (true, true) in 
  let a01 = (false, true) in
  let a10 = (true, false) in
  let a11 = (false, false) in
    [[evaluator e a00; evaluator e a01];
    [evaluator e a10; evaluator e a11]]
let satSolverImplies (e1 : expression) (e2 : expression) evaluator : bool = 
  let e = Or(Not(e1), e2) in
  let result = truthTable2D e evaluator in
  result = [[true;true];[true;true]]
let satSolverImpliedBy (e1 : expression) (e2: expression) evaluator : bool = 
  let e = Or(Not(e2), e1) in
  let result = truthTable2D e evaluator in
  result = [[true;true];[true;true]]
let satSolverIff (e1 : expression) (e2: expression) evaluator : bool = 
  (satSolverImplies e1 e2 evaluator) && (satSolverImpliedBy e1 e2 evaluator)


(*SOLUTION SET*)
let existsSolution (e: expression) evaluator : bool = 
  evaluator e (true, true) || evaluator e (false, true) || evaluator e (true, false) || evaluator e (false, false)
let alwaysTrue (e: expression) evaluator : bool =
  evaluator e (true, true) && evaluator e (false, true) && evaluator e (true, false) && evaluator e (false, false)
let findSolutions (e: expression) evaluator : (bool * bool) list =
  let comb = [(true, true);(false, true);(true, false);(false, false)] in
  let rec findComb comb acc = match comb with
  |[] -> acc
  |h::t -> if evaluator e h then findComb t (h::acc) else findComb t acc 
in findComb comb []

 
(*    TESTINGGGGGG   *)
let test1 = And(Not(Or(Var("a"), And(Var("a"), Var("b")))), Or(And(Var("a"), Var("b")), And(Not(Var("a")), Var("b"))));;
let test2 = And(Not(Var("a")), Var("b")) ;;

let sat1 = And(Var("a"), Var("b"));;
let sat2 = Var("a")
let sat3 = Or(Var("a"), Var("b"))

let test e = 
  let a = evaluateExpression e (true, true) in 
  let a' = evaluateExpression' e (true, true) in 
  let a'' = memoEvaluateExpression e (true, true) in
  let c = printExpression e in
  let d = printExpression' e in
  let f = truthTable2D e evaluateExpression in
  let f' = truthTable2D e evaluateExpression' in
  let f'' = truthTable2D e memoEvaluateExpression in
  let g = findSolutions e evaluateExpression in

  "Reg: " ^ string_of_bool a ^ " TR: " ^ string_of_bool a' ^ " MEMO:  " ^ string_of_bool a'' ^
  "   Print Reg: " ^ c ^ " TR: " ^ d ^ "    " ^ "REG:  " ^string_of_bool_list_list f ^
  " TR : " ^ string_of_bool_list_list f' ^ " MEMO : " ^ string_of_bool_list_list f'' ^
  "FIND SOL: " ^ string_of_2_bool_list g;;

test test1;;
test test2;;
test sat1;;
test sat2;;
test sat3;;
truthTable2D test2 evaluateExpression;;
satSolverImplies sat1 sat3 evaluateExpression;;

memoEvaluateExpression test1 (false, true);;
existsSolution test1 evaluateExpression;;
truthTable2D test1 evaluateExpression;;
findSolutions test1 evaluateExpression

(*evaluateExpression test2 (true, true);;
evaluateExpression' test2 (true, true);;
printExpression test2;;
printExpression' test2;;
truthTable2D test2;;*)