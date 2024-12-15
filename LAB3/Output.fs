module Output

let printResults (results: seq<string * seq<float * float>>) =
    results
    |> Seq.iter (fun (description, points) ->
        printfn "%s" description

        let formattedPoints = 
            points 
            |> Seq.map (fun (x, y) -> sprintf "%.3f\t%.3f" x y)
            |> String.concat "\n"

        printfn "%s" formattedPoints
        printfn "" 
    )
