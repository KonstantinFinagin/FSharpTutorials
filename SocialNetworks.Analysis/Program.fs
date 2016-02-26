open FSharp.Data
open MathNet.Numerics.LinearAlgebra

type Users = JsonProvider<"fsharporgNodes.json">
let userNames = Users.Load("fsharporgNodes.json")

type Connections = JsonProvider<"fsharporgLinks.json">
let userLinks = Connections.Load("fsharporgLinks.json")

let idToName = dict [ for node in userNames.Nodes -> node.Id, node.Name.ToString() ]
let nameToId = dict [ for node in userNames.Nodes -> node.Name.ToString(), node.Id]

// helper functions for helper ids
let idxToId, idToIdx =
    let idxList, idList = 
        userNames.Nodes
        |> Seq.mapi (fun idx node -> (idx, node.Id), (node.Id, idx))
        |> Seq.toList
        |> List.unzip

    dict idxList, dict idList

let idxToIdName idx =
    let id = idxToId.[idx]
    id, idToName.[id]

let nameToIdx screenName =
    let id = nameToId.[screenName]
    idToIdx.[id]

//let a = nameToIdx "dsyme"
let nodeCount = userNames.Nodes.Length

let links = 
    seq { for link in userLinks.Links -> link.Source, link.Target, 1.0 }
    |> SparseMatrix.ofSeqi nodeCount nodeCount


0
