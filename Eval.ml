module VTable = Map.Make(String)
let rec eval_exp vtable exp =
    match exp with
    | Ast.Application (f, exps) ->
        let f' = eval_exp vtable f in
        let exps' = List.map (eval_exp vtable) exps in
        (match f' with
        | Ast.Lambda (vars, defns, exprs) ->
            let vtab = List.fold_left2 (fun vt k v -> VTable.add k v vt) vtable vars exps' in
            eval_exp vtab (List.hd exprs)
        | _ -> Ast.Empty)
    | Ast.If (e1, e2, None) ->
        let e1' = eval_exp vtable e1 in
        (match e1' with
        | Ast.Constant (Ast.Boolean true) -> eval_exp vtable e2
        | Ast.Constant (Ast.Boolean false) -> Ast.Empty
        | _ -> Ast.Empty)
    | Ast.If (e1, e2, Some e3) -> 
        let e1' = eval_exp vtable e1 in
        (match e1' with
        | Ast.Constant (Ast.Boolean true) -> eval_exp vtable e2
        | Ast.Constant (Ast.Boolean false) -> eval_exp vtable e3
        | _ -> Ast.Empty)
    | Ast.Variable s -> VTable.find s vtable
    | _ -> exp
