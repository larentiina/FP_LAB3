module Evaluation

open Interpolation

let maxWindowLength = 4



let minPointsAlg = 
    Map.ofList [
        "linear", 2
        "lagrange", 4
    ]

let evaluateInterpolation (algorithms: string list) (rate: float) (points: seq<float * float>) =

    let algorithmHandlers =
        Map.ofList [
            "linear", (fun pts -> interpolateLinear pts rate)
            "lagrange", (fun pts -> interpolateLagrange pts rate)
        ]
    
    seq {
        let accumulatedPoints = 
                points
                |> Seq.scan (fun acc point -> point :: acc) []
                |> Seq.map List.rev

        for pointsSet in accumulatedPoints do
            for alg in algorithms do
                match algorithmHandlers.TryFind alg with
                | Some handler ->
                    match minPointsAlg.TryFind alg with
                    | Some minPoints ->
                        if Seq.length pointsSet >= minPoints then
                            yield (alg, handler pointsSet)
                    | None -> ()
                | None -> ()
    }
