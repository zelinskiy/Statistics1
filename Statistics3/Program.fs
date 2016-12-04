open System
open System.Windows.Forms
open System.Windows.Forms.DataVisualization.Charting

type LineChartForm( title, xs : (float*float) seq ) =
    inherit Form( Text=title )

    let chart = new Chart(Dock=DockStyle.Fill)
    let area = new ChartArea(Name="Area1")
    let series = new Series()
    do series.ChartType <- SeriesChartType.Line
    do xs |> Seq.iter (series.Points.AddXY >> ignore)
    do series.ChartArea <- "Area1"
    do chart.Series.Add( series )
    do chart.ChartAreas.Add(area)
    do base.Controls.Add( chart )


let R = (new Random()).NextDouble


let flattenTuples tuples = List.collect (fun (a,b) -> [a;b]) tuples 

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

let count step (xs:float list) = 
    let min = List.min xs |> Math.Floor
    let max = List.max xs |> Math.Ceiling
    [min .. step .. max - step] 
        |> List.map (fun s -> s, List.filter (fun x -> x >= s && x <= s + step) xs |> List.length |> float)

let gaussian mean dev = 
    let theta = 2.0 * Math.PI * R()
    let rho = Math.Sqrt(-2.0 * Math.Log(1.0 - R()))
    let scale = dev * rho
    let x = mean + scale * Math.Cos(theta)
    let y = mean + scale * Math.Sin(theta)
    x, y



[<EntryPoint>]
let rec main argv = 
    //let data = seq { for i in 1..1000 do yield sin(float i / 100.0) }
    let data1 = 
        boxMuller 50000 
        |> count 0.05
    let f = new LineChartForm( "Sine", data1 )
    Application.Run(f)
    0