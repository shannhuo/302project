
type expression =
  | Var of string
  | Not of expression
  | And of expression * expression
  | Or of expression * expression;;

let store : (expression, bool) Hashtbl.t = Hashtbl.create 1000
exception Msg of string
let invalidInput = Msg("invalidInput (use \"a\" or \"b\")")

let memoEvaluateExpression (e: expression) (i : bool * bool) : bool = 
  let (a, b) = i in
  let rec memoRec e = 
    match e with 
    |Var("a") -> a
    |Var("b") -> b
    |Not(u) -> (match Hashtbl.find_opt store u with 
      | None -> let a = memoRec u in (Hashtbl.add store u a; not a) 
      | Some(a) -> not a )
    |And(u, v) -> (match Hashtbl.find_opt store u with 
      | None -> let a = memoRec u in (Hashtbl.add store u a; 
        (match Hashtbl.find_opt store v with
          |None -> let b = memoRec v in (Hashtbl.add store u b; a && b)
          |Some(b) -> a && b
        ))
        |Some(a) -> (match Hashtbl.find_opt store v with
        |None -> let b = memoRec v in (Hashtbl.add store u b; a && b)
        |Some(b) -> a && b
        )
      )
    |Or(u, v) -> (match Hashtbl.find_opt store u with 
    | None -> let a = memoRec u in (Hashtbl.add store u a; 
      (match Hashtbl.find_opt store v with
        |None -> let b = memoRec v in (Hashtbl.add store u b; a || b)
        |Some(b) -> a && b
      ))
      |Some(a) -> (match Hashtbl.find_opt store v with
      |None -> let b = memoRec v in (Hashtbl.add store u b; a || b)
      |Some(b) -> a && b
      )
    )
    |Var(_) -> raise invalidInput in
    memoRec e


    (*let rec memoEvaluateExpression (e : expression) (i : bool * bool) =
      let (a, b) = i in
      match Hashtbl.find_opt store e with
      | Some result -> result  
      | None ->
            match e with
            | Var("a") -> a
            | Var("b") -> b
            | Not u -> let result = not (memoEvaluateExpression u i) in (Hashtbl.add store (Not(u)) result; result)
            | And (u, v) -> let result = (memoEvaluateExpression u i) && (memoEvaluateExpression v i) in (Hashtbl.add store (Not(e)) result; result)
            | Or (u, v) -> (memoEvaluateExpression u i) || (memoEvaluateExpression v i)
            | Var(_) -> raise invalidInput
          (Hashtbl.add store e result; result)*)