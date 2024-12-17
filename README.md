## Лабораторная работа №3 по F#

Кузенина Валерия P3332

## Алгоритмы
- Линейная интерполяция
- Интеполяция методом Лагранжа


## Реализация алгоритмов
### Linear interpolation
```
let interpolateLinear (points: seq<float * float>) (samplingRate: float) =
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
```
### Lagrange interpolation
```
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
```


## Тесты 
Для линейной интерполяции:
```
[<Fact>]
let ``Linear interpolation between two points with specified step returns correct values`` () =
    let p1 = (0.0, 0.0)
    let p2 = (10.0, 10.0)
    let samplingRate = 5.0

    let result = interpolateLinearSeq [p1; p2] samplingRate |> Seq.toList

    Assert.True(([(0.0, 0.0); (5.0, 5.0); (10.0, 10.0)] = result))

```
Для интерполяции методом Лагранжа:
```
[<Fact>]
let ``Lagrange interpolation for 4 points returns expected values`` () =
    let points = [ (0.0, 0.0); (1.0, 1.0); (2.0, 4.0); (3.0, 9.0) ]
    let result = interpolateLagrangeSeq points 0.5

    let expected = [(0.0, 0.0); (0.5, 0.25); (1.0, 1.0); (1.5, 2.25); (2.0, 4.0); (2.5, 6.25); (3.0, 9.0)]

    Assert.True((expected = (result |> Seq.toList)))
```
### Пример работы программы
```
\FP\FP3\lab3> dotnet run -alg=linear,lagrange -rate=1
Enter point
1 1
Enter point
2 2
linear
1.000   2.000
1.000   2.000

Enter point
10 15
linear
2.000   3.000   4.000   5.000   6.000   7.000   8.000   9.000   10.000
2.000   3.625   5.250   6.875   8.500   10.125  11.750  13.375  15.000

Enter point
20 24
linear
10.000  11.000  12.000  13.000  14.000  15.000  16.000  17.000  18.000  19.000  20.000
15.000  15.900  16.800  17.700  18.600  19.500  20.400  21.300  22.200  23.100  24.000

lagrange
1.000   2.000   3.000   4.000   5.000   6.000   7.000   8.000   9.000   10.000  11.000  12.000  13.000  14.000  15.000  16.000  17.000  18.000  19.000  20.000
1.000   2.000   3.220   4.625   6.180   7.851   9.603   11.402  13.212  15.000  16.730  18.368  19.880  21.230  22.384  23.307  23.965  24.323  24.346  24.000
```
## Вывод
В ходе лабораторной работы были реализованы 2 алгоритма интерполяции в функциональном стиле. Также научилась работать с консольным вводом-выводом и ленивыми вычислениями в f#
