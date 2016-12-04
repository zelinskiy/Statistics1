open System.IO
open System
open System.Diagnostics;

//QUESTION : quantile

let readFile (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

//let rec qsort pred xs =
//  match xs with
//  | [] -> []
//  | x :: xs ->
//      let sm = qsort pred (xs |> List.filter (pred x))
//      let lg  = qsort pred (xs |> List.filter (pred x >> not))
//      sm @ [x] @ lg

let genRandom =
    let r = Random()  
    Seq.initInfinite (fun _ -> r.NextDouble() |> double)

let mean (xs:seq<double>) = 
    xs  
    |>Seq.sum  
    |> (*) (1.0 / double (Seq.length xs)) 
    |> double

let dispersion xs = 
    xs
    |> Seq.map (fun x -> (x - (mean xs))**2.0)  
    |> Seq.sum 
    |> (*) (1.0 / double ((Seq.length xs) - 1)) 
    |> double

let deviation (xs:seq<double>) = 
    xs 
    |> dispersion 
    |> Math.Sqrt 
    |> double

let quantile (a:double) (xs:seq<double>) = 
    let index = 
        Seq.length xs 
        |> double 
        |> (*) a 
        |> Math.Floor 
        |> int
        |> (fun i -> if i-1 < 0  then (Seq.length xs) - 1 else i)

    if (a < 0.0 || a > 1.0) 
    then raise (new Exception("incorrect percentile")) 
    else
    xs 
    |> Seq.sort 
    |> fun(ys) -> 
        if (Seq.length ys) % 2 = 0 
        then (((Seq.item index ys) + (Seq.item (index-1) ys)) / 2.0)
        else (Seq.item index ys)
    |> Math.Ceiling
    |> double

let count (xs:seq<double>) (x:double) = 
    Seq.fold (fun a b -> if b = x then a + 1 else a) 0 xs

let moda (xs:seq<double>) = 
    Seq.groupBy (fun x -> count xs x) xs
    |> Seq.sortByDescending (fun (c, ys) -> c)
    |> Seq.item 0
    |> fun (c, ys) -> Seq.distinct ys


let minimum (xs:seq<double>) = 
    Seq.reduce (fun x y -> if x < y then x else y) xs

let maximum (xs:seq<double>) = 
    Seq.reduce (fun x y -> if x > y then x else y) xs

let range (xs:seq<double>) = (maximum xs) - (minimum xs)

let funs = [
    ("Выборочное среднее", mean);
    ("Выборочная дисперсия", dispersion);
    ("Стандартное отклонение", deviation);
    ("Медиана", quantile 0.5);
    ("Максимум", maximum);
    ("Минимум", minimum);
    ("Размах", range);
    ("Квантиль 0.1", quantile 0.1);
    ("Квантиль 0.25", quantile 0.25);
    ("Квантиль 0.5", quantile 0.5);
    ("Квантиль 0.75", quantile 0.75);    
]

[<EntryPoint>]
let main argv = 
    let data0 = readFile "data.txt"
    let data1 = [3; 6; 7; 8; 8; 10; 13; 15; 16; 20]
    let data2 = [1;2]
    let data3 = genRandom |> Seq.take 10000

    let data = 
        data3 
        |> Seq.map double 
        |> Seq.map (fun x -> Math.Round(x,3))
        //*Very important* convert random Seq to List:
        |> Seq.toList

    printfn "Последовательность: %A" (Seq.toList data)
    printfn "Мода: %A" (moda data)
    data |> fun xs -> Seq.iter (fun (desc, f) -> printfn "%A : %A" desc (f xs) ) funs 
    printfn "----------------\nЗавершено"
    

    Console.ReadLine() |> ignore
    0 



