exception Msg of string
let invalidInput = Msg("invalidInput (use \"a\" or \"b\")")

type expression =
  | Var of int
  | Not of expression
  | And of expression * expression
  | Or of expression * expression;;

let string_of_bool_list lst =
  "[" ^ String.concat "; " (List.map string_of_bool lst) ^ "]"
let string_of_2_bool_list lst =
  "[" ^ String.concat "; " (List.map (fun (a, b) -> ("("^string_of_bool a ^ ", " ^ string_of_bool b^")")) lst) ^ "]"
let string_of_bool_list_list lst_lst =
  "[" ^ String.concat "; " (List.map string_of_bool_list lst_lst) ^ "]"
let string_of_2_bool_list_list lst_lst =
  "[" ^ String.concat "; " (List.map string_of_2_bool_list lst_lst) ^ "]"


let rec printExpression (e: expression): string = match e with
  |Var(u) -> string_of_int u
  |Not(u) -> "¬" ^ printExpression u
  |And(u, v) -> "(" ^ printExpression u ^ " ∧ " ^ printExpression v ^ ")"
  |Or(u, v) -> "(" ^ printExpression u ^ " ∨ " ^ printExpression v ^ ")"


let inputList (e: expression): int list = 
  let acc = [] in
  let rec matchInputs e = match e with
    |Var(u) -> 
    |Not(u) -> matchInputs u
    |And(u, v)-> matchInputs u @ matchInputs v
    |Or(u, v)-> matchInputs u @ matchInputs v in
  matchInputs e



let test1 = And(Not(Or(Var(1), And(Var(1), Var(2)))), Or(And(Var(3), Var(2)), And(Not(Var(1)), Var(3))));;
let test2 = And(Not(Var(3)), Var(2)) ;;

printExpression test1;;
printExpression test2;;
inputList test1;;
inputList test2;;