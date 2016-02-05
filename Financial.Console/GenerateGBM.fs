module GenerateGBM

open MathNet.Numerics.Distributions
open MathNet.Numerics.Statistics

let normal = Normal.WithMeanVariance(0.0, 1.0)
normal.RandomSource <- new System.Random()
let m = Array.init 5 (fun _ -> normal.Sample())

let get_dW rnd dt N =
    let dW = Normal.WithMeanVariance(0.0, dt)
    dW.RandomSource <- rnd
    (fun () -> Array.init N (fun _ -> dW.Sample()))

let generate_GBM_paths_by_log rnd S0 r sigma T N M =
    let dt = T / (float N)
    let drift = (r - 0.5 * (sigma**2.0)) * dt
    let generator = get_dW rnd dt N

    Array.init M (fun _ -> generator()
                        |> Array.map (fun dWt -> drift + sigma * dWt)
                        |> Array.scan (+) 0.0
                        |> Array.map (fun x -> S0 * exp(x)) )