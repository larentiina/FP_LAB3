module Evaluation

open Interpolation

let maxWindowLength = 4

let takeLast n sequence =
    sequence |> Seq.skip (max 0 (Seq.length sequence - n)) |> Seq.toList

let evaluateInterpolation (algorithms: string list) (rate: float) (points: seq<float * float>) =
    seq {
        let accumulatedPoints =
            points
            |> Seq.scan (fun acc point -> 
                if List.length acc >= maxWindowLength then acc.Tail @ [point] else acc @ [point]) []

        for pointsSet in accumulatedPoints do
            for alg in algorithms do
                match alg with
                | "linear" when List.length pointsSet >= 2 ->
                    let linearPoints = takeLast 2 pointsSet
                    yield ("linear", interpolateLinearSeq linearPoints rate)

                | "lagrange" when List.length pointsSet >= 4 ->
                    yield ("lagrange", interpolateLagrangeSeq pointsSet rate)

                | _ -> ()
    }
