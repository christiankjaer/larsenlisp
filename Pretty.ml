open Ast
let rec print_expr tree =
    match tree with
    | Ast.Constant c -> print_const c
    | Ast.Variable s -> s
    | Ast.If (e1, e2) -> "(if " ^ print_expr e1 ^ " " ^ print_expr e2 ^ ")"
    | Ast.IfElse (e1, e2, e3) -> "(if " ^ print_expr e1 ^ " " ^ print_expr e2 ^ " " ^ print_expr e3 ^ ")"
and print_const c =
    match c with
    | Ast.Number n -> string_of_int n
    | Ast.Boolean true -> "true"
    | Ast.Boolean false -> "false"
    | Ast.Character c -> "'" ^ (String.make 1 c) ^ "'"
    | Ast.String s -> "\"" ^ s ^ "\""
