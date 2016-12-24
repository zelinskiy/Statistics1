module Datasets

open System.IO

let readFile (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}


let everyN n seq = 
  seq |> Seq.mapi (fun i el -> el, i)
      |> Seq.filter (fun (el, i) -> i % n = n - 1)
      |> Seq.map fst  

let LINENORMAL = [1.0 .. 1.01 .. 50.0] |> List.map (fun x -> (x, 5.0 + x*2.0))

let TESTDATA1 = [
    1.0, 200.0;
    2.0, 250.0;
    4.0, 300.0
    ]

let HEIGHTS = 
    readFile "husbandsWifes.txt" 
    |> Seq.map(fun s -> s.Split '\t') 
    |> Seq.map (fun s ->(float s.[0], float s.[1]))

let DOWJONES = 
    readFile "dowjones.txt" 
    |> Seq.map(fun s -> s.Split '\t') 
    |> Seq.map (fun s ->s.[0].Split '/' |> Array.map float, float s.[1])
    |> Seq.map (fun (x,y) -> x.[0] + x.[1] * 31.0, y)

let WINESRAW = 
    readFile "wine.txt" 
    |> Seq.take 500
    |> Seq.map(fun s -> s.Split ';') 
    |> Seq.concat
    |> Seq.map float
let WINES = Seq.zip (everyN 3 WINESRAW) (everyN 7 WINESRAW)

let readclass c = match c with
                    | "1st" -> 1.0
                    | "2nd" -> 2.0
                    | "3rd" -> 3.0
                    | _ -> 4.0

let readsex s = match s with
                    | "male" -> 10.0
                    | "female" -> 20.0
                    | _ -> 3.0

//0id, 1name, 2class, 3age, 4sex, 5surv, 6sexcode
let TITANIC = 
    readFile "titanic.txt" 
    |> Seq.map(fun s -> s.Split '\t') 
    |> Seq.map (fun s ->((readclass s.[2]) + (readsex s.[6]), float s.[5]))

//id\tone\tage\tpressure
let PRESSURE = 
    readFile "pressure.txt" 
    |> Seq.map(fun s -> s.Split '\t') 
    |> Seq.map (fun s ->(float s.[2], float s.[3]))

//Index\tInhabitants\tPercent with incomes below $5000\tPercent unemployed\tMurders per annum per 1,000,000 inhabitants
let MURDERS = 
    readFile "murders.txt" 
    |> Seq.map(fun s -> s.Split '\t') 
    |> Seq.map (fun s ->(float s.[2], float s.[3]))