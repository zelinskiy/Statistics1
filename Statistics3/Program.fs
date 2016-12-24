open System
open System.Windows.Forms
open System.Windows.Forms.DataVisualization.Charting
open System.Diagnostics


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

let _uniform a b = R() * (b - a) + a
let uniformDistribution a b n = [for x in 1 .. n do yield _uniform a b ]


let _centralLimit() = List.sum [for x in 1 .. 12 do yield R() ] - 6.0

let centralLimit mu sigma n = 
    [for x in 1 .. n do yield _centralLimit() ] 
    |> List.map (fun x -> mu + x * sigma )

let rec _boxMuller() = 
    let x = R() * (if R() > 0.5 then 1.0 else -1.0)
    let y = R() * (if R() > 0.5 then 1.0 else -1.0)
    let s = x * x + y * y
    if s <= 0.0 || s > 1.0 then _boxMuller() else
    let t = Math.Sqrt(-2.0 * Math.Log(s) / s)
    x * t, y * t 

let boxMuller mu sigma n = 
    [for x in 1 .. n / 2 do yield _boxMuller() ] 
    |> flattenTuples
    |> List.map (fun x -> mu + x * sigma)

let _exponential a b = -Math.Log(_uniform a b)

//!!!!!!WTF!!!!!!!
let exponential  n = 
    uniformDistribution 0.0 1.0 n 
    |> List.map (fun x -> -Math.Log(x) * 0.1)
    |> List.filter (fun x -> x <= 1.0 )
//!!!!!WTF!!!!!!!!

let (stairWidth : float array) = Array.zeroCreate 257
let (stairHeight : float array) = Array.zeroCreate 256

let x1 = 3.6541528853610088;
let A = 4.92867323399e-3;

let STEPS = pown 10 3

let MAX_RAND = pown 10 9 |> float

let setupBoxes() = 
    stairHeight.[0] <- Math.Exp(-0.5 * x1 * x1)
    stairWidth.[0] <- A / stairHeight.[0];
    stairWidth.[256] <- 0.0;
    for i in 1..255 do
        stairWidth.[i] <- Math.Sqrt(-2.0 * Math.Log(stairHeight.[i - 1]));
        stairHeight.[i] <- stairHeight.[i - 1] + (A / stairWidth.[i]);


let rec _normalZiggurat iter (x:float) = 
    if iter > STEPS - 1 then x 
    else
        let mutable B = _uniform 0.0 MAX_RAND |> int
        let mutable stairId = B &&& 255
        let mutable x = _uniform 0.0 stairWidth.[stairId]

        if (x < stairWidth.[stairId + 1])
        then if B > (MAX_RAND / 2.0 |> int) then _normalZiggurat STEPS (-1.0 * x) else _normalZiggurat STEPS x
        else if stairId = 0 then
            let mutable z = -1.0
            let mutable y = 0.0
            if z > 0.0 then
                x <- _exponential 0.0 x1
                z <- z - 0.5 * x * x
            else if z <= 0.0 then
                while z <= 0.0 do
                    x <- _exponential 0.0 x1
                    y <- _exponential 0.0 1.0
                    z <- y - 0.5 * x * x
            x <- x + x1
            if B > (MAX_RAND / 2.0 |> int) then _normalZiggurat STEPS (-1.0 * x) else _normalZiggurat STEPS x
        else if (_uniform stairHeight.[stairId - 1] stairHeight.[stairId]) < Math.Exp(-0.5 * x * x) then
            if B > (MAX_RAND / 2.0 |> int) then _normalZiggurat STEPS (-1.0 * x) else _normalZiggurat STEPS x
        else
            _normalZiggurat (iter + 1) x

let rec __normalZiggurat() = _normalZiggurat 0 0.0
          
let normalZiggurat mu sigma n = 
    setupBoxes()
    [for x in 1 .. n do yield __normalZiggurat() ]
    |> List.map (fun x -> mu + x * sigma )

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
    let size = 100000
    let step = 0.01    
    countPlot step size "Ziggurat" (normalZiggurat 0.0 1.0)
    countPlot step size "Box-Muller"(boxMuller 0.0 1.0)
    countPlot step size "Central limit theorem" (centralLimit 0.0 1.0)
    //countPlot step size "Uniform " (uniformDistribution 10.0 20.0)
    //countPlot step size "Exponential " exponential 
    0