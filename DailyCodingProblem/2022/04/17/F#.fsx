#r "nuget: FSharpAux"

open FSharpAux

let mapping = Map [
    "2", ["a"; "b"; "c"]
    "3", ["d"; "e"; "f"]
]

let getPossibleCombs input =
    String.toCharArray input
    |> Array.fold (
        fun acc c ->
            acc
            |> List.collect (
                fun ele1 ->
                    Map.find (string c) mapping
                    |> List.map (fun ele2 -> $"{ele1}{ele2}")
            )
    ) [""]

getPossibleCombs "23"
getPossibleCombs "2332"

let getPossibleCombs' input =
    String.toCharArray input
    |> Array.fold (
        fun (acc : string list) c ->
            let table = Map.find (string c) mapping
            List.init acc.Length (
                fun i1 ->
                    List.init table.Length (fun i2 -> $"{acc.[i1]}{table.[i2]}")
            )
            |> List.concat
    ) [""]

getPossibleCombs' "23"
getPossibleCombs' "2332"