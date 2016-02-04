open FSharp.Charting

open System
open System.Drawing
open GenerateGBM

[<EntryPoint>]
let main args =

    let T = 0.25
    let M = 3
    let N = 200

    let S0 = 50.0
    let sigma = 0.2
    let r = 0.01

    // generate a chart object for each price path
    let plot_path (T:float) (N:int) (path:float array) color =
        let dt = T / (float N)
        path 
        |> Array.mapi (fun n p -> ((float n)*dt, p))
        |> Chart.Line
        |> Chart.WithStyling(Color = color, BorderWidth = 2)

    let rnd = new System.Random()
    
    let paths = generate_GBM_paths_by_log rnd S0 r sigma T N M

    // determine maximum and minimum for the Y-axis
    let mx, mn = paths |> Array.fold (fun (mx, mn) p -> (max mx (Array.max p), min mn (Array.min p))) (Double.MinValue, Double.MaxValue)

    let colors = [| Color.Green; Color.Red; Color.Blue |]
    
    let path_charts = Array.map2 (plot_path T N) paths colors

    let title = sprintf "3 simulated GBM paths with S0=%.2f, r=%.2f, sigma=%.2f, T=%.2f, N=%d" S0 r sigma T N

    let chart = Chart.Combine path_charts
                |> Chart.WithStyling(Margin=(2.0, 12.0, 2.0, 2.0))
                |> Chart.WithTitle(Text=title, 
                                    FontName="Arial", 
                                    FontSize=14.0, 
                                    FontStyle=FontStyle.Bold, 
                                    InsideArea = false)
                |> Chart.WithXAxis(Title="time in years", 
                                    Max=T, 
                                    Min=0.0, 
                                    TitleAlignment = StringAlignment.Center)
                |> Chart.WithYAxis(Title="price in $", 
                                    Max=(Math.Round(mx) + 1.0), 
                                    Min=(Math.Round(mn)-1.0), 
                                    TitleAlignment=StringAlignment.Center, 
                                    TitleFontName="Arial",
                                    TitleFontSize=14.0,
                                    TitleFontStyle=FontStyle.Bold)

    chart.ShowChart();

    System.Windows.Forms.Application.Run()

    0


