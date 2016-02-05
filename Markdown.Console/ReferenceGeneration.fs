module ReferenceGeneration

open Types
open Helper

let rec generateSpanRefs (refs:ResizeArray<_>) = 
    function
    | Hyperlink(body, url) as span ->
        let id = sprintf "[%d]" (refs.Count + 1)
        refs.Add(id, url)
        [span; Literal(id)]
    | Matching.SpanNode(shape, children) ->
        let children = children |> List.collect (generateSpanRefs refs)
        [Matching.SpanNode(shape, children)]
    | span -> [span]

let generateBlockRefs refs = 
    function
    | Matching.BlockNode(shape, children) ->
        let children = children |> List.collect (generateSpanRefs refs)
        Matching.BlockNode(shape, children)
    | block -> block

