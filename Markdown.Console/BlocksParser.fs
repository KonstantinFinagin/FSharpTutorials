module BlockParser

module List =    
    // walks over a list
    let partitionWhile f =
        let rec loop acc = function
            // taking elements that match a given predicate
            // adding result to accumulator if success match
            | x::xs when f x -> loop (x::acc) xs
            // and returning as the first element of the resulting tuple
            | xs -> List.rev acc, xs
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