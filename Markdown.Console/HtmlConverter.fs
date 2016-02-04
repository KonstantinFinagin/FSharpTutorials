module HtmlConverter

open System.IO
open Types

let outputElement (output:TextWriter) tag attributes body =
    let attrString =
        //join the attributes into a single string
        [ for k, v in attributes -> k + "=\"" + v + "\""]
        |> String.concat " "
    output.Write("<" + tag + attrString + ">")
    // run the function that generates the body
    body()
    output.Write("</" + tag + ">")

let rec formatSpan (output:TextWriter) = 
    let out = outputElement output
    let iter spans = (fun () -> spans |> List.iter(formatSpan output))
    function   
    | Literal(str) ->
        output.Write(str)
    | Strong(spans) -> 
        out "strong" [] (spans |> iter)
    | Emphasis(spans) ->
        out "em" [] (spans |> iter)
    | Hyperlink(spans, url) ->
        out "a" ["href", url] (spans |> iter)
    | InlineCode(code) ->
        output.Write("<code>" + code + "</code>")
    | HardLineBreak ->
        output.Write("<br />")

let rec formatBlock (output:TextWriter) =
    let out = outputElement output
    let iter spans = (fun () -> spans |> List.iter(formatSpan output))
    function
    | Heading(size, spans) ->
        out ("h" + size.ToString()) [] (spans |> iter)
    | Paragraph(spans) ->
        out "p" [] (spans |> iter)
    | CodeBlock(lines) ->
        out "pre" [] (fun () -> lines |> List.iter output.WriteLine)
    | BlockQuote(blocks) ->
        out "quote" [] (fun () -> blocks |> List.iter(formatBlock output))


