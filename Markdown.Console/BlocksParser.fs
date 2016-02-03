module BlockParser

open Types
open Parser

// extending module List
module List =    
    // walks over a list
    let partitionWhile predicate =
        let rec loop accumulator = function
            // taking elements that match a given predicate
            // adding result to accumulator if success match
            | firstline::restlines when predicate firstline -> loop (firstline::accumulator) restlines
            // and returning as the first element of the resulting tuple
            | restlines -> List.rev accumulator, restlines
        loop []

// takes a list of lines 
let (|PrefixedLines|) prefix (lines:list<string>) =     
    // and collects all lines that start with a given prefix
    let prefixed, other = 
        lines |> List.partitionWhile (fun line ->
            line.StartsWith(prefix))
    // removes the prefix from the found lines and returns them together with
    // the remaining lines
    [ for line in prefixed ->
        line.Substring(prefix.Length) ], other

// splits input lines into two lists using the first blank line as a separator
let (|LineSeparated|) lines =
    let isWhite = System.String.IsNullOrWhiteSpace
    // forward composition operator
    match List.partitionWhile (isWhite >> not) lines with
    | par, _::rest
    | par, ([] as rest) -> par, rest

let (|AsCharList|) (str:string) =
    List.ofSeq str

let (|Heading'|_|) = 
    function
    | AsCharList(StartsWith ['#'; ' '] heading)::lines -> Some(1, heading, lines)
    | AsCharList(StartsWith ['#'; '#'; ' '] heading)::lines -> Some(2, heading, lines)
    | heading::AsCharList(StartsWith ['=';'=';'='] _)::lines -> Some(1, heading |> List.ofSeq, lines)
    | heading::AsCharList(StartsWith ['-';'-';'-'] _)::lines -> Some(2, heading |> List.ofSeq, lines)
    | _ -> None
     

let rec parseBlocks lines = seq {
    match lines with
    // first-level heading
    // :: is a standard pattern for lists used to match the first line
    | Heading'(size, heading, lines) ->
        yield Heading(size, parseSpans [] heading |> List.ofSeq)
        yield! parseBlocks lines
    | PrefixedLines "    " (body, lines) when body <> [] ->
        yield CodeBlock(body)
        yield! parseBlocks lines
    // recognises paragraphs
    | LineSeparated (body, lines) when body <> [] ->
        // concatenate lines of the paragraph and use parseSpans to handle formatting in the paragraph
        let body = String.concat " " body |> List.ofSeq
        yield Paragraph(parseSpans[] body |> List.ofSeq)
        yield! parseBlocks lines
    // line-by-line quotation
    | PrefixedLines ">" (body, lines) when body<> [] ->
        let body = body |> List.map(fun s -> s.Substring(1))
        yield BlockQuote(parseBlocks body |> List.ofSeq)
        yield! parseBlocks lines
    // multiline quotations todo
    
    // skip blank lines
    | line::lines when System.String.IsNullOrWhiteSpace(line) ->
        yield! parseBlocks lines
    | _ -> ()
}
















