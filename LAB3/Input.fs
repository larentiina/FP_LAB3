module Input

open System.IO
open System


let readPoints (input: TextReader) =
    seq {
        while true do
            printfn "Введите точку"
            let line: string = input.ReadLine()

            if line = null then
                yield! []
            else
                match line.Split(separator = [| ' '; '\t' |], options = StringSplitOptions.RemoveEmptyEntries) with
                | [| x; y |] -> yield (float x, float y)
                | _ -> printfn "Неправильный формат данных"
    }
