open FSharp.Data

type Users = JsonProvider<"fsharporgNodes.json">
let userNames = Users.Load("fsharporgNodes.json")

type Connections = JsonProvider<"fsharporgLinks.json">
let userLinks = Connections.Load("fsharporgLinks.json")


0