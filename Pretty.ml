open Ast
let rec print_expr tree =
    match tree with
    | Ast.Constant c -> print_const c
    | Ast.Variable s -> s
    | Ast.Lambda (fs, defns, exps) -> 
            let print_formals formals =
                match formals with
                | [f] -> f
                | _ -> "("^ String.concat " " formals ^")"
            in
            "(lambda " ^ print_formals fs ^ " " ^ (String.concat " " (List.map print_expr exps))^ ")"
    | Ast.If (e1, e2, None) -> "(if " ^ print_expr e1 ^ " " ^ print_expr e2 ^ ")"
    | Ast.If (e1, e2, Some e3) -> "(if " ^ print_expr e1 ^ " " ^ print_expr e2 ^ " " ^ print_expr e3 ^ ")"
    | Ast.Application (e, exps) -> "(" ^ print_expr e ^ " " ^ (String.concat " " (List.map print_expr exps)) ^ ")"
and print_const c =
    match c with
    | Ast.Number n -> string_of_int n
    | Ast.Boolean true -> "true"
    | Ast.Boolean false -> "false"
    | Ast.Character c -> "#'" ^ (String.make 1 c) ^ "'"
    | Ast.String s -> "\"" ^ s ^ "\""
