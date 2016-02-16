open FSharp.Data.Toolbox.Twitter


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

    

    System.Windows.Forms.Application.Run()

    0 // return an integer exit code
