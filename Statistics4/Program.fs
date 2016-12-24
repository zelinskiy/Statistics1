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


let test i = 
    let alpha = 0.00001
    let iters = 300
    let X = PRESSURE
    let Y = []
    let tetas = (GD iters alpha) X
    let t0 = fst tetas
    let t1 = snd tetas
    printfn "%A %A" t0 t1
    let f = new ChartForm(  
                    "Value",                    
                    "Count",
                    SeriesChartType.Point,
                    X,
                    Y,
                    H t0 t1)
    Application.Run(f)

[<EntryPoint>]
let main argv = 
    
    //printfn "%A" TITANIC        
    test 1
    //Console.ReadKey()
    0 
