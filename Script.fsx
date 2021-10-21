#r "nuget: XPlot.Plotly"

open XPlot.Plotly

[ 1 .. 10 ] |> Chart.Line |> Chart.Show