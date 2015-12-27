type token =
    | Identifier of string
    | String of string
    | Character of char
    | Number of int
    | Lp
    | Rp

let rec lex = parser
    | [< ' (' ' | '\n' | '\r' | '\t'); stream >] -> lex stream
    | [< ' ('a' .. 'z' |'!'|'$'|'%'|'&'|'*'|
        '/'|':'|'<'|'='|'>'|'?'|'~'|'_'|'^' as c); stream >] ->
            let buffer = Buffer.create 1 in
            Buffer.add_char buffer c;
            lex_identifier buffer stream
    | [< ' ('+' | '-' as c); stream >] ->
            [< 'Identifier (String.make 1 c); lex stream >]
    | [< ''('; stream >] -> [< 'Lp; lex stream >]
    | [< '')'; stream >] -> [< 'Rp; lex stream >]
    | [< ''\"'; stream >] -> 
            let buffer = Buffer.create 1 in
            lex_string buffer stream
    | [< ' ('0' .. '9' as c); stream >] ->
            let buffer = Buffer.create 1 in
            Buffer.add_char buffer c;
            lex_number buffer stream
    | [< ''#'; 'c; stream >] -> [< 'Character c; lex stream >]
    | [< >] -> [< >]

and lex_string buffer = parser
    | [< ''\"'; stream >] ->
            [< 'String (Buffer.contents buffer); lex stream >]
    | [< 'c; stream >] ->
            Buffer.add_char buffer c;
            lex_string buffer stream

and lex_number buffer = parser
    |  [< ' ('0' .. '9' as c); stream >] ->
            Buffer.add_char buffer c;
            lex_number buffer stream
    | [< stream >] ->
            [< 'Number (int_of_string (Buffer.contents buffer)); lex stream >]

and lex_identifier buffer = parser
    | [< ' ('a' .. 'z' | '0' .. '9' | '!'|'$'|'%'|'&'|'*'|
        '/'|':'|'<'|'='|'>'|'?'|'~'|'_'|'^'|'.'|'+'|'-' as c); stream >] ->
            Buffer.add_char buffer c;
            lex_identifier buffer stream
    | [< stream >] ->
            [< 'Identifier (Buffer.contents buffer); lex stream >]
