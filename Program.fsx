#r "nuget: XPlot.Plotly"

open System
open XPlot.Plotly

let getSignal pointCount n phi =
    sin(2.0 * Math.PI * n / pointCount + phi / 180.0 * Math.PI)

let getSignalPeriod pointCount =
    seq { 3 * pointCount / 4 .. 2 * pointCount }

let getDoubleSignals x = (x |> Seq.sumBy (fun x -> x * x), x |> Seq.sum)

let getSignalsResult (a, b) m = (0.707 - Math.Sqrt(a / (m + 1.0)), 0.707 - Math.Sqrt(a / (m + 1.0) - Math.Pow(b / (m + 1.0), 2.0)))

let signals m pointCount phi =
    seq { for n in 0 .. m do getSignal (float pointCount) (float n) phi } 
    |> getDoubleSignals
    |> getSignalsResult <| float m

let getFirst (a, _) = a
let getSecond (_, b) = b

let getSignals s pointcount phi =
    seq {
        for n in s do
            yield! seq { signals n pointcount phi}
    }

let trace1 pointCount phi =
    Scatter(
        x = getSignalPeriod pointCount,
        y = (getSignals (getSignalPeriod pointCount) pointCount phi |> Seq.map (fun x -> x |> getFirst))
    )

let trace2 pointCount phi =
    Scatter(
        x = getSignalPeriod pointCount,
        y = (getSignals (getSignalPeriod pointCount) pointCount phi |> Seq.map (fun x -> x |> getSecond))
    )

let getN = 2048
let getPhi = (Math.PI * 2.0 / 4.0)


[<EntryPoint>]
let main argv =
    [trace1 getN getPhi; trace2 getN getPhi] |> Chart.Plot |> Chart.WithHeight 700 |> Chart.WithWidth 1200 |> Chart.Show
    0