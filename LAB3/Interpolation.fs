module Interpolation
let interpolateLagrange (points: seq<float * float>) (samplingRate: float) =
    let pointsList = points |> Seq.toList
    let n = pointsList.Length
    let xs, ys = pointsList |> List.unzip
    let step = samplingRate
    let minX = List.head xs
    let maxX = List.last xs  + step*(if ((xs[n - 1] - minX) % step = 0) then 0.0 else 1.0)

    seq {
        for x in seq{minX .. step .. maxX} do
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
    }




let interpolateLinear (points: seq<float * float>) (samplingRate: float) =
    if Seq.length points < 2 then
        failwith "Linear interpolation requires at least 2 points"

    seq {
        for (x1, y1), (x2, y2) in Seq.pairwise points do
            let rec generatePoints x =
                seq {
                    if x < x2+samplingRate then
                        let t = (x - x1) / (x2 - x1)
                        let y = y1 + t * (y2 - y1)
                        yield (x, y)
                        yield! generatePoints (x + samplingRate)
                }
            yield! generatePoints x1
    }

