open System
open System.Windows.Forms
open System.Windows.Forms.DataVisualization.Charting

type ChartForm( title, xtitle, ytitle, ptype:SeriesChartType, xs : (float*float) seq ) =
    inherit Form( Text=title )

    let chart = new Chart(Dock=DockStyle.Fill)
    let area = new ChartArea(Name="Area1")
    let series = new Series()
    do series.ChartType <- ptype
    do xs |> Seq.iter (series.Points.AddXY >> ignore)
    do series.ChartArea <- "Area1"

    do area.AxisX.Title <- xtitle
    do area.AxisY.Title <- ytitle

    do chart.Series.Add( series )
    do chart.ChartAreas.Add(area)
    do base.Controls.Add( chart )
    


let R = (new Random()).NextDouble

let count step (xs:float list) = 
    let min = List.min xs |> Math.Floor
    let max = List.max xs |> Math.Ceiling
    [min .. step .. max - step] 
        |> List.map (fun s -> s, List.filter (fun x -> x >= s && x <= s + step) xs |> List.length |> float)

let flattenTuples tuples = List.collect (fun (a,b) -> [a;b]) tuples 


let uniformDistribution a b n = [for x in 1 .. n do yield R() * (b - a) + a ]


let centralLimit (r:unit -> float) = 
    ([for x in 1 .. 12 do yield r() ] |> List.sum) - 6.0


let rec _boxMuller (r:unit -> float) = 
    let x = r() * (if r() > 0.5 then 1.0 else -1.0)
    let y = r() * (if r() > 0.5 then 1.0 else -1.0)
    let s = x * x + y * y
    if s <= 0.0 || s > 1.0 then _boxMuller r else
    let z0 = x * Math.Sqrt(-2.0 * Math.Log(s) / s)
    let z1 = y * Math.Sqrt(-2.0 * Math.Log(s) / s)
    z0, z1

let boxMuller n = flattenTuples [for x in 1 .. n / 2 do yield _boxMuller R ]


let exponential n = [for x in 1 .. n / 2 do yield -Math.Log(R()) ]




let countPlot step size name func = 
    let data = 
        func size 
        |> count step
    let f = new ChartForm( 
                    name + " " + step.ToString(), 
                    "Count",                    
                    "Value",
                    SeriesChartType.Line,
                    data)
    Application.Run(f)


let allPlot step size name func =
    let data = func size |> List.mapi (fun i x -> (x, i |> float))
    let f = new ChartForm( 
                    name + " " + step.ToString(), 
                    "Index",                    
                    "Value",
                    SeriesChartType.Point,
                    data)
    Application.Run(f)


[<EntryPoint>]
let rec main argv = 
    let size = 1000
    let step = 0.1
    countPlot step size "Gaussian" boxMuller
    //gaussianPlot step size

    0