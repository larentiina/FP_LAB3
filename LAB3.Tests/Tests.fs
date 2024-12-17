module Tests

open System
open Xunit
open Interpolation


[<Fact>]
let ``Linear interpolation between two points with specified step returns correct values`` () =
    let p1 = (0.0, 0.0)
    let p2 = (10.0, 10.0)
    let samplingRate = 5.0

    let result = interpolateLinear [p1; p2] samplingRate |> Seq.toList

    Assert.True(([(0.0, 0.0); (5.0, 5.0); (10.0, 10.0)] = result))


[<Fact>]
let ``Linear interpolation should include the last point in the sequence`` () =
    let p1 = (0.0, 0.0)
    let p2 = (10.1, 10.0)
    let samplingRate = 0.5

    let result = interpolateLinear [p1; p2] samplingRate |> Seq.toList

    Assert.True(fst p2 <= fst (List.last result))


[<Fact>]
let ``Lagrange interpolation for 4 points returns expected values`` () =
    let points = [ (0.0, 0.0); (1.0, 1.0); (2.0, 4.0); (3.0, 9.0) ]
    let result = interpolateLagrange points 0.5

    let expected = [(0.0, 0.0); (0.5, 0.25); (1.0, 1.0); (1.5, 2.25); (2.0, 4.0); (2.5, 6.25); (3.0, 9.0)]

    Assert.True((expected = (result |> Seq.toList)))


[<Fact>]
let ``Lagrange interpolation with negative values returns correct results`` () =
    let points = [ (-2.0, -8.0); (-1.0, -1.0); (1.0, 1.0); (2.0, 8.0) ]
    let result = interpolateLagrange points 1.0

    let expected = [(-2.0, -8.0); (-1.0, -1.0); (0.0, 0.0); (1.0, 1.0); (2.0, 8.0)]

    Assert.True((expected = (result |> Seq.toList)))

[<Fact>]
let ``Lagrange interpolation should include the last point in the sequence`` () =
    let points = [ (1.0, 2.0); (3.0, 3.0); (4.0, 5.0); (6.1, 7.0) ]
    let result = interpolateLagrange points 0.5 |> Seq.toList

    Assert.True(fst (List.last points) <= fst (List.last result))
