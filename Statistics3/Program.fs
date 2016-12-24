open System
open System.Windows.Forms
open System.Windows.Forms.DataVisualization.Charting
open System.Diagnostics


type ChartForm( title, xtitle, ytitle, ptype:SeriesChartType,
                            xs : (float*float) seq,
                            realxs: (float*float) seq ) =
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

    let series2 = new Series()
    do series2.ChartType <- SeriesChartType.Line
    do realxs |> Seq.iter (series2.Points.AddXY >> ignore)
    do series2.ChartArea <- "Area1"


    do chart.Series.Add( series2 )


    do base.Controls.Add( chart )
    

let erf (_x:float) =
    let a1 = 0.254829592
    let a2 = -0.284496736
    let a3 = 1.421413741
    let a4 = -1.453152027
    let a5 = 1.061405429
    let p = 0.3275911

    let sign = Math.Sign _x
    let x = Math.Abs _x
    let t = 1.0 / (1.0 + p*x)
    let y = 1.0 - (((((a5*t + a4)*t) + a3)*t + a2)*t + a1)*t*Math.Exp(-x*x);
    (float sign) * y


let R = (new Random()).NextDouble

let pureGauss sigma mu (x:float) = 
    (1.0/(sigma * Math.Sqrt(2.0 * Math.PI))) *
    Math.Pow(Math.E, ((-1.0 * (x - mu) * (x - mu)) / (2.0 * sigma * sigma)))

let generatePureGauss sigma mu a b step = [for x in a..step..b do yield (x, pureGauss sigma mu x)]

let generatePureExponential lambda a b (step:float) = 
    [for x in (a + step)..step..(b - step) do yield (x, 1.0 - Math.Pow(Math.E, -lambda * x) )]

let generatePureNormal sigma mu a b (step:float) = 
    [for x in (a + step)..step..(b - step) do yield (x, 0.5 * (1.0 + (erf ((x - mu) / Math.Sqrt(2.0 * sigma * sigma) )) ) )]


let count step (xs:float list) = 
    let min = List.min xs |> Math.Floor
    let max = List.max xs |> Math.Ceiling
    [min .. step .. max] 
        |> List.map (fun s -> s, 
            (List.filter (fun x -> x >= s && x <= s + step) xs 
                |> List.length 
                |> float) )

let summa step (xs:float list) = 
    let min = List.min xs |> Math.Floor
    let max = List.max xs |> Math.Ceiling
    [min .. step .. max] 
        |> List.map (fun s -> s, 
                                xs
                                |> List.filter (fun x -> x >= s && x <= s + step)
                                |> List.map Math.Abs
                                |> List.sum)


let flattenTuples tuples = List.collect (fun (a,b) -> [a;b]) tuples 

let _uniform a b = R() * (b - a) + a
let uniformDistribution a b n = [for x in 1 .. n do yield _uniform a b ]


let _centralLimit() = List.sum [for x in 1 .. 12 do yield R() ] - 6.0

let centralLimit sigma mu n = 
    [for x in 1 .. n do yield _centralLimit() ] 
    |> List.map (fun x -> mu + x * sigma)

let rec _boxMuller() = 
    let x = R() * (if R() > 0.5 then 1.0 else -1.0)
    let y = R() * (if R() > 0.5 then 1.0 else -1.0)
    let s = x * x + y * y
    if s <= 0.0 || s > 1.0 then _boxMuller() else
    let t = Math.Sqrt(-2.0 * Math.Log(s) / s)
    x * t, y * t 

let boxMuller sigma mu n = 
    [for x in 1 .. n / 2 do yield _boxMuller() ] 
    |> flattenTuples
    |> List.map (fun x -> mu + x * sigma)

let _exponential a b = -Math.Log(_uniform a b)

//!!!
let exponential lambda n = 
    uniformDistribution 0.0 1.0 n 
    |> List.map (fun x -> -Math.Log(x) / lambda )
    |> List.filter (fun x -> x <= 1.0 )

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
        then if B > (MAX_RAND / 2.0 |> int) 
                    then _normalZiggurat STEPS (-1.0 * x) 
                    else _normalZiggurat STEPS x
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
            if B > (MAX_RAND / 2.0 |> int) 
                then _normalZiggurat STEPS (-1.0 * x) 
                else _normalZiggurat STEPS x
        else if (_uniform stairHeight.[stairId - 1] stairHeight.[stairId]) < Math.Exp(-0.5 * x * x) then
            if B > (MAX_RAND / 2.0 |> int) 
                then _normalZiggurat STEPS (-1.0 * x) 
                else _normalZiggurat STEPS x
        else
            _normalZiggurat (iter + 1) x

let rec __normalZiggurat() = _normalZiggurat 0 0.0
          
let normalZiggurat sigma mu n = 
    setupBoxes()
    [for x in 1 .. n do yield __normalZiggurat() ]
    |> List.map (fun x -> mu + x * sigma )


let funcPlot step (size:int) name func realfunc coeff= 
    let data = 
        func size 
        |> List.sort
        |> List.mapi (fun i x -> (x, (float i) / (float size)))    
    
    let realData = 
        realfunc (List.min data |> fst) (List.max data |> fst) step
        |> Seq.toList

    let f = new ChartForm( 
                    name + " " + step.ToString(), 
                    "Value",                    
                    "Count",
                    SeriesChartType.Point,
                    data,
                    realData
                    )
    Application.Run(f)

let func1Plot step (size:int) name func= 
    let data = 
        func size 
        |> List.sort
        |> List.mapi (fun i x -> (x, (float i) / (float size)))    
    
    let f = new ChartForm( 
                    name + " " + step.ToString(), 
                    "Value",                    
                    "Count",
                    SeriesChartType.Point,
                    data,
                    []
                    )
    Application.Run(f)


let countPlot step (size:int) name func= 
    let data = 
        func size 
        |> count step
    let f = new ChartForm( 
                    name + " " + step.ToString(), 
                    "Value",                    
                    "Count",
                    SeriesChartType.BoxPlot,
                    data,
                    [])
    Application.Run(f)

let count2Plot step (size:int) name func realfunc coeff = 
    let data = 
        func size 
        |> count step
    let realData = 
        realfunc (List.min data |> fst) (List.max data |> fst) step
        |> Seq.toList
        |> List.map (fun (x, y) -> (x, y * ((float size) * coeff)))
    let f = new ChartForm( 
                    name + " " + step.ToString(), 
                    "Value",                    
                    "Count",
                    SeriesChartType.BoxPlot,
                    data,
                    realData
                    )
    Application.Run(f)



//let sumPlot step size name func realfunc = 
//    let data = 
//        func size 
//        |> summa step
//    let realData = 
//        realfunc (List.min data |> fst) (List.max data |> fst) step 
//        |> Seq.toList
//        |> List.map (fun (x, y) -> (x, y * ((float size) / 200.0)))
//    let f = new ChartForm( 
//                    name + " " + step.ToString(), 
//                    "Value",                    
//                    "Sum",
//                    SeriesChartType.BoxPlot,
//                    data,
//                    realData)
//    Application.Run(f)


//let allPlot step size name func realfunc =
//    let data = func size |> List.mapi (fun i x -> (x, i |> float))
//    let realData = 
//        realfunc (List.min data |> fst) (List.max data |> fst) step 
//        |> Seq.toList
//        |> List.map (fun (x, y) -> (x, y * ((float size) / 200.0)))
//    let f = new ChartForm( 
//                    name + " " + step.ToString(), 
//                    "Value",                    
//                    "Index",
//                    SeriesChartType.Point,
//                    data,
//                    realData)
//    Application.Run(f)



[<EntryPoint>]
let rec main argv = 
    let size = 100
    let step = 0.05
    let sigma = 1.0
    let mu = 0.0  
    let lambda = 1.0
    let a = 10.0
    let b = 20.0
    let func = true
    if func then  
        funcPlot step size "Ziggurat" (normalZiggurat sigma mu) (generatePureNormal sigma mu) step
        funcPlot step size "Box-Muller" (boxMuller sigma mu) (generatePureNormal sigma mu) step
        funcPlot step size "Central limit theorem" (centralLimit sigma mu) (generatePureNormal sigma mu) step
        func1Plot step size "Uniform " (uniformDistribution 10.0 20.0)
        funcPlot step size "Exponential " (exponential lambda) (generatePureExponential lambda) step
    else
        countPlot step size "Ziggurat" (normalZiggurat sigma mu)
        countPlot step size "Box-Muller" (boxMuller sigma mu)
        countPlot step size "Central limit theorem" (centralLimit sigma mu)
        countPlot step size "Uniform " (uniformDistribution 10.0 20.0)
        countPlot step size "Exponential " (exponential 10.0)
    0