open System
open System.IO
open System.Windows.Forms.DataVisualization.Charting


open Datasets
open MyCharting
open System.Windows.Forms


let rescale X:float seq = 
    let _min = Seq.min X
    let _max = Seq.max X
    Seq.map (fun x -> (x - _min) / (_max - _min)) X

let rescale2 data:((float*float) seq) = 
    let X = data |> Seq.map fst |> rescale
    let Y = data |> Seq.map snd |> rescale
    Seq.zip X Y

let H t0 t1 x = t0 + t1 * x * 1.0

let J t0 t1 (data: (float*float) seq) = 
    let m = Seq.length data |> float
    let summation = (fun di -> Math.Abs( (snd di) - (H t0 t1 (fst di)) ))
    (1.0/m) * (data |> Seq.sumBy summation )

let rec _GD t0 t1 i alpha (data: (float*float) seq) = 
    if i = 0 then (t0, t1) else
    let m = Seq.length data |> float
    let h = H t0 t1
    let cost0 = fun d -> (h (fst d)) - (snd d)
    let cost1 d = (cost0 d) * (fst d)
    let _t0 = t0 - (alpha / m) * (Seq.sumBy cost0  data)
    let _t1 = t1 - (alpha / m) * (Seq.sumBy cost1  data)
    //printfn "%A %A" _t0 _t1
    _GD _t0 _t1 (i-1) alpha data 

let GD:int -> float -> (float*float) seq -> (float*float) = _GD 0.0 0.0

let lincor X Y = 
    let mx = Seq.average X
    let my = Seq.average Y
    let a =Seq.sumBy (fun (x, y) -> (x-mx) * (y-my)) (Seq.zip X Y)
    let b1 = Math.Sqrt(Seq.sumBy (fun x -> (x - mx) ** 2.0 ) X)
    let b2 = Math.Sqrt(Seq.sumBy (fun y -> (y - my) ** 2.0 ) Y)
    a / (b1 * b2)

let determ h data =
    let mean = Seq.averageBy snd data
    let SSres = Seq.sumBy (fun d -> (h(fst d) - mean) ** 2.0) data
    let SStot = Seq.sumBy (fun d -> ((fst d) - mean) ** 2.0) data
    Math.Sqrt(SSres / SStot)

let test i = 
    let alpha = 0.00001
    let iters = 500
    let X = LINENORMAL
    let Xx = Seq.map fst X
    let Xy = Seq.map snd X
    let Y = []
    let tetas = (GD iters alpha) X
    let t0 = fst tetas
    let t1 = snd tetas
    let h = H t0 t1
    printfn "theta0 = %A; theta1 = %A" t0 t1
    printfn "corr = %A; deter = %A" (lincor Xx Xy) (determ h X)
    let f = new ChartForm(  
                    "X",                    
                    "Y",
                    SeriesChartType.Point,
                    X,
                    Y,
                    H t0 t1)
    Application.Run(f)



[<EntryPoint>]
let main argv = 
    //let X = Seq.map fst TITANIC
    //let Y = Seq.map snd TITANIC
    //printfn "%A" (lincor X X)
    test 1
    //Console.ReadKey()
    0 
