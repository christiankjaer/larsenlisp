type expr =
    | Empty
    | Constant of const
    | Variable of string
    | Lambda of string list * defn list * expr list
    | If of expr * expr * expr option
    | Application of expr * expr list

and const =
    | Number of int
    | Boolean of bool
    | Character of char
    | String of string

and defn =
    | Define of string * expr
