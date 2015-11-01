module BinomialDistribution.Program

let Binomial x n p = 
    let f = float
    let essentialyZero = 10.E-12

    let m = (f(n) * p) |> truncate |> int

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

    let rec Calculate k totalUnscaledProbability previous unscaled = 
        let current = CalcCurrent previous k
        let totalUnscaledProbability' = totalUnscaledProbability + current
        let unscaled' = CalcUnscaled x k unscaled current

        if Done current then
            unscaled', totalUnscaledProbability'
        else 
            Calculate (NextK k) totalUnscaledProbability' current unscaled'

    let InitialUnscaled = if (m <= x) then 1.0 else 0.0

    let UnscaledResultAboveM, TotalUnscaledProbabilityAboveM = 
        Calculate(m+1) 1.0 1.0 InitialUnscaled
    let UnscaledResult, TotalUnscaledProbability =
        Calculate(m-1) TotalUnscaledProbabilityAboveM 1.0 UnscaledResultAboveM

    UnscaledResult / TotalUnscaledProbability













