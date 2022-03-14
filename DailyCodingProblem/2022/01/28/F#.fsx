#r "nuget: FSharpAux"

open FSharpAux

let getProductWithoutI arr =
    arr 
    |> Array.mapi (
        fun i _ ->
            arr
            |> Array.foldi (
                fun i2 acc v2 ->
                    if i = i2 then acc
                    else acc * v2
            ) 1
    )

getProductWithoutI [|1 .. 5|]
getProductWithoutI [|3; 2; 1|]

let getProductWithoutI' (arr : int []) = [|
    for i = 0 to arr.Length - 1 do
        let mutable acc = 1
        for i2 = 0 to arr.Length - 1 do
            if i2 <> i then acc <- acc * arr.[i2]
        yield acc
|]
getProductWithoutI' [|1 .. 5|]
getProductWithoutI' [|3; 2; 1|]