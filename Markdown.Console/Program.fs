open Parser
open System

[<EntryPoint>]
let main args =
    let result ="hello world! [http://lenta.ru](Lenta hyperlink)" |> List.ofSeq |> parseSpans [] |> List.ofSeq
    let rln = Console.ReadLine()
    0
