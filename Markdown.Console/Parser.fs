module Parser

open Types

let toString chars = 
    System.String(chars |> Array.ofList)

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

let (|StartsWith|_|) prefix input = 
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

let (|Delimited|_|) delim = parseBracketed delim delim

let (|Bracketed|_|) opening closing = parseBracketed opening closing

let rec parseSpans acc chars = seq {
    // sequence expression that emits Literal 
    // from the accumulated input provided it's not empty
    let emitLiteral = seq {
        if acc <> [] then 
            yield acc |> List.rev |> toString |> Literal }

    match chars with 
    | StartsWith [' ';' ';'\r';'\n'] chars ->
        // first emit a literal
        yield! emitLiteral
        // emit currently recognised span
        yield HardLineBreak
        // continue parsing recursively
        yield! parseSpans [] chars
    | StartsWith [' ';' ';'\r'] chars ->
        yield! emitLiteral
        yield HardLineBreak
        yield! parseSpans [] chars
    | StartsWith [' ';' ';'\n'] chars ->
        yield! emitLiteral
        yield HardLineBreak
        yield! parseSpans [] chars
    // (body, chars) is a pattern that defines two symbols to hold the delimited body and remaining input
    | Delimited ['`'] (body, chars) ->
        yield! emitLiteral
        yield InlineCode(toString body)
        yield! parseSpans [] chars
    | Delimited ['*'; '*'] (body, chars)
    | Delimited ['_'; '_'] (body, chars) ->
        yield! emitLiteral
        yield Strong(parseSpans [] body |> List.ofSeq)
        yield! parseSpans [] chars
    | Delimited ['*'] (body, chars)
    | Delimited ['_'] (body, chars) ->
        yield! emitLiteral
        yield Emphasis(parseSpans [] body |> List.ofSeq)
        yield! parseSpans [] chars
    | Bracketed ['['] [']'] (body, Bracketed ['('] [')'] (url, chars)) ->
        yield! emitLiteral
        yield Hyperlink(parseSpans [] body |> List.ofSeq, toString url)
        yield! parseSpans [] chars
    // take the current character, add it to a list of accumulated characters, continue with processing
    | c::chars ->
        yield! parseSpans (c::acc) chars
    // emit the last literal
    | [] ->
        yield! emitLiteral
}
