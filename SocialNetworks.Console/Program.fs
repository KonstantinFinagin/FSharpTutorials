open FSharp.Data.Toolbox.Twitter
open System.Threading
open FSharp.Data
open System.IO


[<EntryPoint>]
let main argv = 
    
    let key = "RZClDkP8rj06E0wB4G09rDX9b"
    let secret = "vSzrFhDadmgFY6QdMfouBWMX5AWsGhkHFKSbG3qRBliB5RC0nf"

    let twitter = Twitter.AuthenticateAppOnly(key, secret)

    let friends = twitter.Connections.FriendsIds(screenName="@fsharporg")
    let followers = twitter.Connections.FollowerIds(screenName="@fsharporg")

    let idsOfInterest = Seq.append friends.Ids followers.Ids |> set

    let groupedIds =
        idsOfInterest
        |> Seq.mapi (fun i id -> (i/100, id))
        |> Seq.groupBy fst

    let twitterNodes = [| for _, group in groupedIds do
                            let ids = Seq.map snd group
                            let nodeInfo = 
                                twitter.Users.Lookup(ids)
                                |> Array.map (fun node -> node.Id, node.ScreenName)
                            yield! nodeInfo |]

    let isInNetwork id = idsOfInterest.Contains id

    let twitterConnections (ids : int64 seq) = [|
        for srcId in ids do
            Thread.Sleep(60000)
            let connections = 
                try 
                    twitter.Connections.FriendsIds(srcId).Ids
                    |> Array.filter isInNetwork
                with _ -> [||]
            yield! connections |> Seq.map (fun tgtId -> srcId, tgtId)
    |]

    let jsonNode (userInfo : int64*string) = 
        let id, name = userInfo
        JsonValue.Record [|
            "name", JsonValue.String name
            "id", JsonValue.Number (decimal id)
        |]

    let jsonNodes = 
        let nodes = twitterNodes |> Array.map jsonNode
        [| "nodes", (JsonValue.Array nodes) |] 
        |> JsonValue.Record

    File.WriteAllText("fSharpOrgNodes.json", jsonNodes.ToString())


    System.Windows.Forms.Application.Run()

    0 // return an integer exit code
