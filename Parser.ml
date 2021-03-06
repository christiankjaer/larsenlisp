let rec parse_expr = parser
    | [< 'Lexer.Number n >] -> Ast.Constant (Ast.Number n)
    | [< 'Lexer.String s >] -> Ast.Constant (Ast.String s)
    | [< 'Lexer.Character c >] -> Ast.Constant (Ast.Character c)
    | [< 'Lexer.Lp; e=parse_special_form; 'Lexer.Rp >] -> e
    | [< 'Lexer.Identifier "true" >] -> Ast.Constant (Ast.Boolean true)
    | [< 'Lexer.Identifier "false" >] -> Ast.Constant (Ast.Boolean false)
    | [< 'Lexer.Identifier s >] -> Ast.Variable s
        
and parse_special_form = parser
    | [< 'Lexer.Identifier "if"; exps=parse_exps [] >] -> 
        (match exps with
        | [e1;e2] -> Ast.If (e1, e2, None)
        | [e1;e2;e3] -> Ast.If (e1, e2, Some e3)
        | _ -> raise (Stream.Error "2 or 3 exps expected"))
    | [< 'Lexer.Identifier "lambda"; formals=parse_formals; exps=parse_exps [] >] ->
             Ast.Lambda (formals, [], exps)
    | [< e=parse_expr; exps=parse_exps [] >] ->
            Ast.Application (e, exps)

and parse_exps acc = parser
    | [< e=parse_expr; stream >] -> parse_exps (e::acc) stream
    | [< >] -> List.rev acc
and parse_formals = parser
    | [< 'Lexer.Identifier s >] -> [s]
    | [< 'Lexer.Lp; stream >] ->
            let rec parse_f acc = parser
                | [< 'Lexer.Identifier s; stream >] -> parse_f (s::acc) stream
                | [< 'Lexer.Rp >] -> List.rev acc
            in
            parse_f [] stream
    | [< >] -> []

let parse_defn = parser
    | [< 'Lexer.Lp; 'Lexer.Identifier "define"; 'Lexer.Identifier v; e=parse_expr; 'Lexer.Rp >] ->
            Ast.Define (v, e)

(* let main () =
    let s = Sys.argv.(1) in
    let l = Lexer.lex (Stream.of_string s) in
    print_string (Pretty.print_expr (parse_expr l));;

main () ;; *)
