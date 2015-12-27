open Ast

let rec parse_expr = parser
    | [< 'Lexer.Number n >] -> Constant (Number n)
    | [< 'Lexer.String s >] -> Constant (String s)
    | [< 'Lexer.Character c >] -> Constant (Character c)
    | [< 'Lexer.Lp; e=parse_special_form; 'Lexer.Rp >] -> e
        
and parse_special_form = parser
    | [< 'Lexer.Identifier "if"; exps=parse_exps [] >] -> 
        (match exps with
        | [e1;e2] -> If (e2, e1, None)
        | [e1;e2;e3] -> If (e3, e2, Some e1)
        | _ -> raise (Stream.Error "2 or 3 exps expected"))
    | [< 'Lexer.Identifier "lambda"; formals=parse_formals; exps=parse_exps [] >] ->
             Lambda (formals, [], exps)

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

let main () =
    let s = Sys.argv.(1) in
    let l = Lexer.lex (Stream.of_string s) in
    print_string (Pretty.print_expr (parse_expr l));;

main () ;;
