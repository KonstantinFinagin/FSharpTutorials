module Helper

open Types

module Matching =
    let (|SpanNode|_|) span = 
        match span with 
        | Strong spans 
        | Emphasis spans 
        | Hyperlink(spans, _) ->
            Some(box span, spans)
        | _ -> None

    let SpanNode (span:obj, children) = 
        match unbox span with
        | Strong _ -> Strong children
        | Emphasis _ -> Emphasis children
        | Hyperlink(_, url) -> Hyperlink(children, url)
        | _ -> invalidArg "" "Incorrect MarkdownSpan"

    let (|BlockNode|_|) block =
        match block with
        | Heading(_, spans)
        | Paragraph(spans) -> Some(box block, spans)
        | _ -> None

    let BlockNode (block:obj, spans) = 
        match unbox block with
        | Heading(a, _) -> Heading(a, spans)
        | Paragraph (_) -> Paragraph(spans)
        | _ -> invalidArg "" "Incorrect MarkdownBlock"