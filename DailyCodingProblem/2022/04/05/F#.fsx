// brute force attempt
let returnLargestProduct input =
    input
    |> List.collect (
        fun i1 ->
            input
            |> List.collect (
                fun i2 ->
                    input
                    |> List.map (fun i3 -> i1 * i2 * i3)
            )
    )
    |> List.max

let testList = [-10; -10; 5; 2]

returnLargestProduct testList

// sorting attempt
open System

let returnLargestProduct' input =
    let biggestPositiveNumbers = List.sortDescending input
    let biggestNegativeNumbers = List.sortByDescending (fun (v : int) -> Math.Abs v) input
    if Math.Abs biggestNegativeNumbers[0] > biggestPositiveNumbers[1] || Math.Abs biggestNegativeNumbers[1] > biggestPositiveNumbers[2] then
        biggestNegativeNumbers[0] * biggestNegativeNumbers[1] * biggestPositiveNumbers.Head
    else biggestPositiveNumbers[0] * biggestPositiveNumbers[1] * biggestPositiveNumbers[2]

returnLargestProduct' testList

// fastest attempt
let returnLargestProduct'' input =
    let mutable highestPosNo    = 0
    let mutable sndHighestPosNo = 0
    let mutable trdHighestPosNo = 0
    let mutable highestNegNo    = 0
    let mutable sndHighestNegNo = 0
    let checkPos v =
        match highestPosNo, sndHighestPosNo, trdHighestPosNo with
        | x, y, z when x > y && y > z   -> trdHighestPosNo  <- Math.Max(trdHighestPosNo, v)
        | x, y, z when x > z && z > y   -> sndHighestPosNo  <- Math.Max(sndHighestPosNo, v)
        | _                             -> highestPosNo     <- Math.Max(highestPosNo, v)
    let checkNeg v =
        match highestNegNo, sndHighestNegNo with
        | x, y when x < y   -> sndHighestNegNo  <- Math.Min(v, sndHighestNegNo)
        | _                 -> highestNegNo     <- Math.Min(v, highestNegNo)
    for i in input do
        checkPos i
        checkNeg i
    if Math.Abs highestNegNo > sndHighestPosNo || Math.Abs sndHighestNegNo > trdHighestPosNo then
        highestNegNo * sndHighestNegNo * highestPosNo
    else highestPosNo * sndHighestPosNo * trdHighestPosNo

returnLargestProduct'' testList