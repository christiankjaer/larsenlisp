open Ast

let rec parse_expr = parser
    | [< 'Lexer.Number n >] -> Constant (Number n)
    | [< 'Lexer.String s >] -> Constant (String s)
    | [< 'Lexer.Character c >] -> Constant (Character c)
    | [< 'Lexer.Lp; 'Lexer.Identifier "if";
         e1=parse_expr; e2=parse_expr; e3=parse_expr;
         'Lexer.Rp >] ->
            IfElse (e1, e2, e3)
    | [< 'Lexer.Lp; 'Lexer.Identifier "if"; e1=parse_expr; e2=parse_expr; 'Lexer.Rp >] ->
            If (e1, e2)

let main () =
    let s = Sys.argv.(1) in
    let l = Lexer.lex (Stream.of_string s) in
    print_string (Pretty.print_expr (parse_expr l));;

main () ;;
