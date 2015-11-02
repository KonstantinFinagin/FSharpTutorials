module Parser

open Types

let rec parseInlineBody acc = function
    | '`'::rest -> 
        Some(List.rev acc, rest)
    | c::chars ->
        parseInlineBody (c::acc) chars
    | [] -> None

let parseInline = function
    | '`'::chars ->
        parseInlineBody [] chars
    | _ -> None