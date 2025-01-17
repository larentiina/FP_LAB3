module Interpolation

let takeLast n sequence =
    sequence |> Seq.skip (max 0 (Seq.length sequence - n)) |> Seq.toList

let interpolateLagrange (points: seq<float * float>) (samplingRate: float) =
    let pointsList = takeLast 4 points |> Seq.toList
    let n = pointsList.Length

    let x3 = if n >= 3 then Some (List.item 2 pointsList) else None

    let xs, ys = pointsList |> List.unzip
    let step = samplingRate
    let minX = List.head xs
    let maxX = List.last xs 

    seq {
        for x in seq {minX .. step .. maxX} do
            match (points |> Seq.tryHead, pointsList |> List.tryHead, x3) with
            | (Some (x1, _), Some (x2, _), Some (x3, _)) when x1 <> x2 && x <= x3 -> ()
            | _ -> 
                let y =
                    [ for i in 0 .. n - 1 do
                           let xi, yi = xs.[i], ys.[i]

                           let li =
                               [ for j in 0 .. n - 1 do
                                      if j <> i then (x - xs.[j]) / (xi - xs.[j]) else 1.0 ]
                               |> List.fold (*) 1.0

                           yield yi * li ]
                    |> List.sum

                yield (x, y)

        let lastPoint = List.last pointsList
        if not (Seq.exists (fun x -> x = fst  lastPoint) (seq { minX .. step .. maxX })) then
            yield lastPoint
    }








let interpolateLinear (points: seq<float * float>) (samplingRate: float) =
    let newpoints = takeLast 2 points
    seq {
        for (x1, y1), (x2, y2) in Seq.pairwise newpoints do
            let rec generatePoints x =
                seq {
                    if x < x2 then
                        let t = (x - x1) / (x2 - x1)
                        let y = y1 + t * (y2 - y1)

                        match Seq.tryHead points with
                        | Some (fx, fy) when (x1, y1) <> (fx, fy) && x <= x1 -> ()
                        | _ -> yield (x, y)

                        yield! generatePoints (x + samplingRate)

                }
            yield! generatePoints x1

            yield (x2, y2)
    }

