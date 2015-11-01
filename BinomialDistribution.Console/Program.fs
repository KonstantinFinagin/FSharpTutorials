module BinomialDistribution.Program

let Binomial x n p = 
    let f = float
    let cumulative = true
    let mutable totalUnscaledP = 0.
    let mutable unscaledResult = 0.
    let essentialyZero = 10.E-12

    let m = (f(n) * p) |> truncate |> int

    totalUnscaledP <- totalUnscaledP + 1.

    if m = x then unscaledResult <- unscaledResult + 1.
    if cumulative && m < x then unscaledResult <- unscaledResult + 1.
    let mutable previousValue = 1.
    let mutable isDone = false

    let mutable k = m + 1

    while not isDone && k <= n do
        let currentValue = previousValue * f(n - k + 1) * p / (f(k) * (1. - p))
        totalUnscaledP <- totalUnscaledP + currentValue
        
        if k = x then unscaledResult <- unscaledResult + currentValue
        if cumulative && k < x then unscaledResult <- unscaledResult + currentValue
        if currentValue <= essentialyZero then isDone <- true
        previousValue <- currentValue
        k <- k + 1

    previousValue <- 1.
    isDone <- false
    k <- m - 1

    while not isDone && k >= 0 do
        let currentValue = previousValue * f(k + 1) * (1. - p) / (f(n - k) * p)
        totalUnscaledP <- totalUnscaledP + currentValue
        
        if k = x then unscaledResult <- unscaledResult + currentValue
        if cumulative && k < x then unscaledResult <- unscaledResult + currentValue

        if currentValue <= essentialyZero then isDone <- true
        previousValue <- currentValue
        k <- k - 1

    unscaledResult / totalUnscaledP













