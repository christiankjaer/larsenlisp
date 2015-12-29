let rec eval_exp exp =
    match exp with
    | Ast.If (e1, e2, None) ->
        let e1' = eval_exp e1 in
        (match e1' with
        | Ast.Constant (Ast.Boolean true) -> eval_exp e2
        | Ast.Constant (Ast.Boolean false) -> Ast.Empty
        | _ -> Ast.Empty)
    | Ast.If (e1, e2, Some e3) -> 
        let e1' = eval_exp e1 in
        (match e1' with
        | Ast.Constant (Ast.Boolean true) -> eval_exp e2
        | Ast.Constant (Ast.Boolean false) -> eval_exp e3
        | _ -> Ast.Empty)
    | _ -> exp
