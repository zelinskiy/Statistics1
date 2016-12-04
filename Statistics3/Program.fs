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

    //do series.Points.AddXY(1.0, Seq.maxBy (fun (x, y) -> y) xs |> snd ) |> ignore
    //do series.Points.AddXY(0.0, Seq.maxBy (fun (x, y) -> y) xs |> snd) |> ignore

    do chart.Series.Add( series )
    do chart.ChartAreas.Add(area)
    do base.Controls.Add( chart )
    


let R = (new Random()).NextDouble

let count step (xs:float list) = 
    let min = List.min xs |> Math.Floor
    let max = List.max xs |> Math.Ceiling
    [min .. step .. max] 
        |> List.map (fun s -> s, List.filter (fun x -> x >= s && x <= s + step) xs |> List.length |> float)

let flattenTuples tuples = List.collect (fun (a,b) -> [a;b]) tuples 


let uniformDistribution a b n = [for x in 1 .. n do yield R() * (b - a) + a ]


let _centralLimit() = List.sum [for x in 1 .. 12 do yield R() ] - 6.0

let centralLimit n = 
    [for x in 1 .. n do yield _centralLimit() ] 
    |> List.map (fun x -> 0.5 + x / 10.0)

let rec _boxMuller() = 
    let x = R() * (if R() > 0.5 then 1.0 else -1.0)
    let y = R() * (if R() > 0.5 then 1.0 else -1.0)
    let s = x * x + y * y
    if s <= 0.0 || s > 1.0 then _boxMuller() else
    let t = Math.Sqrt(-2.0 * Math.Log(s) / s)
    x * t, y * t 

let boxMuller n = 
    [for x in 1 .. n / 2 do yield _boxMuller() ] 
    |> flattenTuples
    |> List.map (fun x -> 0.5 + x / 10.0)


//!!!!!!WTF!!!!!!!
let exponential n = 
    uniformDistribution 0.0 1.0 n 
    |> List.map (fun x -> -Math.Log(x) / 4.0 )
    |> List.filter (fun x -> x <= 1.0 )
//!!!!!WTF!!!!!!!!


let countPlot step size name func = 
    let data = 
        func size 
        |> count step
    let f = new ChartForm( 
                    name + " " + step.ToString(), 
                    "Value",                    
                    "Count",
                    SeriesChartType.BoxPlot,
                    data)
    Application.Run(f)


let allPlot step size name func =
    let data = func size |> List.mapi (fun i x -> (x, i |> float))
    let f = new ChartForm( 
                    name + " " + step.ToString(), 
                    "Value",                    
                    "Index",
                    SeriesChartType.Point,
                    data)
    Application.Run(f)


[<EntryPoint>]
let rec main argv = 
    let size = 80000
    let step = 0.01    
    //printfn "%A" ((List.min (exponential size)))
    //printfn "%A" ((List.max (exponential size)) )
    //Console.ReadKey();
    countPlot step size "Exponential" exponential
    0