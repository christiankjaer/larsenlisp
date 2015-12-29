let rec main_loop = parser
    | [< e=Parser.parse_expr >] ->
            print_string (Pretty.print_expr (Eval.eval_exp e));
            print_newline ();
            main ()
    | [< >] -> ()
and main () =
    print_string "ll> "; flush stdout;
    let stream = Lexer.lex (Stream.of_channel stdin) in
    main_loop stream;;
main () ;;
