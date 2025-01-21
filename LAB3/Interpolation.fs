module Interpolation

let takeLast n sequence =
    sequence |> Seq.skip (max 0 (Seq.length sequence - n)) |> Seq.toList

let interpolateLagrange (points: seq<float * float>) (samplingRate: float) =
    let pointsList = takeLast 4 points |> Seq.toList

    let n = pointsList.Length
    let xs, ys = pointsList |> List.unzip
    let step = samplingRate
    let minX = List.head xs
    let maxX = List.last xs
    let initialRemainder = fst (Seq.head points )% step
    let correctedMinX = 
        if minX % step <> initialRemainder then
            minX - (minX % step) + initialRemainder
        else
            minX

    seq {
        for x in seq { correctedMinX .. step .. maxX } do
            if x <= maxX then
                let y =
                    [ for i in 0 .. n - 1 do
                           let xi, yi = xs.[i], ys.[i]

                           let li =
                               [ for j in 0 .. n - 1 do
                                      if j <> i then (x - xs.[j]) / (xi - xs.[j])
                                      else 1.0 ]
                               |> List.fold (*) 1.0

                           yield yi * li ]
                    |> List.sum

                if x> fst pointsList.[2] || fst pointsList.[0] = (fst (Seq.head points))then
                    yield (x, y)
    }





let interpolateLinear (points: seq<float * float>) (samplingRate: float) =
    let getRemainder (x: float) = x % samplingRate

    let newpoints = takeLast 2 points 
    let rate = fst (Seq.head points ) % samplingRate

    seq {
        for (x1, y1), (x2, y2) in Seq.pairwise newpoints do
            let rec generatePoints x =
                seq {
                    let currentRemainder = getRemainder x

                    let adjustedX =
                        if currentRemainder <> rate then
                            x - currentRemainder + rate
                        else
                            x

                    if adjustedX <= x2 then
                        let t = (adjustedX - x1) / (x2 - x1)
                        let y = y1 + t * (y2 - y1)

                        if fst newpoints.[0] < adjustedX || fst newpoints.[0] = fst (Seq.head points ) then 
                            yield (adjustedX, y)

                        yield! generatePoints (adjustedX + samplingRate)
                }

            yield! generatePoints x1
    }

