open System
open System.IO
open Input
open Output
open Evaluation



let parseArgs (args: string array) =
    let defaultAlgorithms = [ "linear" ]
    let defaultSamplingRate = 1.0


    args
    |> Array.fold
        (fun (algorithms, samplingRate) arg ->
            match arg.Split('=') with
            | [| "-alg"; algs |] ->
                (algs.Split(',') |> Array.toList, samplingRate)
            | [| "-rate"; rate |] ->
                (algorithms, float rate)
            | _ -> (algorithms, samplingRate))
        (defaultAlgorithms, defaultSamplingRate)


[<EntryPoint>]
let main (args: string array) =
    let algorithms, samplingRate = parseArgs args
    let points = readPoints Console.In 
    evaluateInterpolation algorithms samplingRate points |> printResults
    0
