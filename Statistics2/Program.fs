open System

let table = seq [
    seq [35.0;30.0;21.0];
    seq [32.0;24.0;22.0];
    seq [31.0;26.0;34.0];
    seq [30.0;20.0;31.0];
]

let groupMeans = [32.0;25.0;27.0];

let q = 4.0;
let p = 3.0;

let mean = table |> Seq.concat |> Seq.average

let totalSum = table |> Seq.concat |> Seq.sumBy (fun x -> (x - mean)**2.0)

let factorSum = q * (Seq.sumBy (fun gm -> (gm - mean) ** 2.0) groupMeans)

let remainderSum = totalSum - factorSum

let factorDisp = factorSum / ( p - 1.0)

let remainderDisp = remainderSum / (p * ( q - 1.0))

let fisherFactor = factorDisp / remainderDisp

let criticalF = 4.26

let zeroHypothesis = fisherFactor < criticalF

[<EntryPoint>]  
let main argv = 
    printfn "Общая сумма %A" totalSum
    printfn "Факторная сумма %A" factorSum
    printfn "Остаточная сумма %A" remainderSum
    printfn "Факторная дисперсия %A" factorDisp
    printfn "Остаточная дисперсия %A" remainderDisp
    printfn "Фактор фишера %A" fisherFactor
    printfn "Критическое значение %A" criticalF
    printfn "гипотеза выполняется %A" zeroHypothesis
    Console.ReadKey() |> ignore
    0