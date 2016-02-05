module PayoffFunctions

open GenerateGBM

open MathNet.Numerics.Distributions
open MathNet.Numerics.Statistics

let S_T (path:float array) = path.[path.Length - 1]                     

let european_call K (path:float array) =                                
    max ((S_T path) - K) 0.0      

let up_and_out_call K H (path:float array) =
    if Array.max path.[1..] >= H then 0.0 
    else european_call K path                            


let simulate_payoffs rnd S0 r sigma T N M payoff = 
    [| for path in generate_GBM_paths_by_log rnd S0 r sigma T N M -> 
            let currentPayoff = payoff path
            (exp (-r*T)) * currentPayoff |]


let price_option rnd S0 r sigma T N M payoff = 
    simulate_payoffs rnd S0 r sigma T N M payoff
    |> Array.average     

let price_option_v2 rnd S0 r sigma T N M payoff = 
    let Ys = simulate_payoffs rnd S0 r sigma T N M payoff
    let C_estimate = Ys |> Array.average
    let Y_var = Ys.Variance()
    let std_error = sqrt(Y_var / (float M))
    (C_estimate, Y_var, std_error)


