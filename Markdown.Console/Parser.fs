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

let (| StartsWith | _ |) prefix input = 
    let rec loop = function
        | p::prefix, r::rest when p = r ->
            loop(prefix, rest)
        | [], rest ->
            Some(rest)
        | _ -> None

    loop (prefix, input)

let rec parseBracketedBody closing acc = function
    | StartsWith closing (rest) -> 
        Some(List.rev acc, rest)
    | c::chars -> 
        parseBracketedBody closing (c::acc) chars
    | _ -> None
   
let parseBracketed opening closing = function
    | StartsWith opening chars ->
        parseBracketedBody closing [] chars
    | _ -> None

let (| Delimited | _ |) delim = parseBracketed delim delim
















  