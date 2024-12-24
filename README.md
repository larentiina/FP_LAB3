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
    let maxX = List.last xs  + step*(if ((xs[n - 1] - minX) % step = 0) then 0.0 else step)

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
Введите точку
0 0
Введите точку
1.571 1
linear
0.000   0.000
1.000   0.637
2.000   1.273

Введите точку
3.142 0
linear
2.000   1.273
1.571   1.000
2.571   0.363
3.571   -0.273

Введите точку
4.712 -1
linear
3.571   -0.273
3.142   0.000
4.142   -0.637
5.142   -1.274

lagrange
0.000   0.000
1.000   0.973
2.000   0.841
3.000   0.120
4.000   -0.674
5.000   -1.026
```
## Вывод
В ходе лабораторной работы были реализованы 2 алгоритма интерполяции в функциональном стиле. Также научилась работать с консольным вводом-выводом и ленивыми вычислениями в f#
