module BinomialDistribution.Program

let Binomial x n p = 
    let f = float
    let mutable totalUnscaledP = 0.
    let mutable unscaledResult = 0.
    let essentialyZero = 10.E-12

    let m = (f(n) * p) |> truncate |> int

    totalUnscaledP <- totalUnscaledP + 1.

    if m = x then unscaledResult <- unscaledResult + 1.


    let CalcCurrent value k = 
        if k > m then 
            value * float(n - k + 1) * p / (float(k) * (1. - p))
        else
            value * float(k + 1) * (1. - p) / (float(n - k) * p)

    let CalcUnscaled x k acc increment =
        if k <= x  then acc + increment
        else acc

    let Done current = current <= essentialyZero

    let NextK k = if k > m then k + 1 else k - 1

    

    let mutable previousValue = 1.
    let mutable isDone = false
    let mutable k = m + 1

    while not isDone && k <= n do
        let currentValue = CalcCurrent previousValue k
        totalUnscaledP <- totalUnscaledP + currentValue        
        unscaledResult <- CalcUnscaled x k unscaledResult currentValue
        isDone <- Done currentValue
        previousValue <- currentValue
        k <- NextK k

    previousValue <- 1.
    isDone <- false
    k <- m - 1

    while not isDone && k >= 0 do
        let currentValue = CalcCurrent previousValue k
        totalUnscaledP <- totalUnscaledP + currentValue
        unscaledResult <- CalcUnscaled x k unscaledResult currentValue
        isDone <- Done currentValue
        previousValue <- currentValue
        k <- NextK k

    unscaledResult / totalUnscaledP













