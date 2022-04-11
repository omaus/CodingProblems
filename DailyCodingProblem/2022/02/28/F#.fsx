#r "nuget: FSharp.Stats"

open FSharp.Stats

let testArray = [|2; 1; 5; 7; 2; 0; 5|]

let inline getRunningMedian input =
    let floatInput = Array.map float input
    [|
        for i = 0 to input.Length - 1 do
        Seq.median (floatInput[0 .. i])
    |]

getRunningMedian testArray