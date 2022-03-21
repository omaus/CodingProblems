// Task:
// A matrix of pixels with zeroes (white pixels) and ones (black pixels). 
// The task is to remove all black pixels that are not connected to a black pixel at the border.
// The borders are the outer rows and columns.
// A connection can be of horizontal or vertical neighboring.
// In the following example, the middle black pixel has a connection to a border black pixel due to horizontal neighboring:
// [[1; 0; 0];
//  [0; 1; 1];
//  [0; 0; 0]]
// A connection is extending, meaning that a black non-border pixel being connected to a black border pixel can serve as a connecting pixel itself:
// [[0; 1; 1; 0];
//  [0; 1; 0; 0];
//  [0; 1; 0; 1];
//  [0; 0; 1; 0]]
// The pixels in the second column build a connection from the top (real border pixel) until the third row.
// taken from: https://www.youtube.com/watch?v=4tYoVx0QoN0#t=1m33s (around minute 1:30, as in timestamp)

#r "nuget: FSharpAux"

open FSharpAux

let input =
    [|
        [|1; 0; 0; 0; 0; 0|]
        [|0; 1; 0; 1; 1; 1|]
        [|0; 0; 1; 0; 1; 0|]
        [|1; 1; 0; 0; 1; 0|]
        [|1; 0; 1; 1; 0; 0|]
        [|1; 0; 0; 0; 0; 1|]
    |]

let arr2dInput = input |> Array2D.ofJaggedArray

// Type representation of the problem.
type Pixel = {
    Value       : int
    IsBorderCon : bool
}

/// Takes a value and converts it into a Pixel.
let convertToPixel v = {Value = v; IsBorderCon = false}

let inputAsPixelArr2d = arr2dInput |> Array2D.map convertToPixel

/// Takes a 2D Pixel array and converts it into a 2D string array with Value = 1 being "X" and Value = 0 being " ".
let stringifyPxArr arr2d =
    arr2d
    |> Array2D.map (
        fun px -> if px.Value = 1 then "X" else " "
    )

/// Takes a 2D Pixel array and removes all non-black Pixels that are not connected to the borders.
let deleteNonBlackPixel arr2d =
    arr2d
    |> Array2D.map (
        fun px -> if px.Value = 1 && px.IsBorderCon then px else {px with Value = 0}
    )

/// Sets a Pixel as being connected to a border.
let setBorderPixel (px : Pixel) = {px with IsBorderCon = true}

/// Takes a 2D Pixel array and sets all Pixels at the borders as being border-connected.
let setBorderPixels (arr2d : Pixel [,]) =
    let maxRowI = arr2d.GetLength(0) - 1
    let maxColI = arr2d.GetLength(1) - 1
    arr2d
    |> Array2D.mapi (
        fun iR iC px ->
            match (iR,iC,px.Value) with
            | _,0,1
            | 0,_,1 -> setBorderPixel px
            | a,b,1 when a = maxRowI || b = maxColI -> setBorderPixel px
            | _ -> px
    )

let inputWithBorderCheckedPixel = inputAsPixelArr2d |> setBorderPixels

/// Checks if any horizontal or vertical neighbor of a Pixel is connected to a border.
let isAnyNeighborBorderCon i j (arr2d : Pixel [,]) =
    arr2d[i - 1,j].IsBorderCon ||
    arr2d[i + 1,j].IsBorderCon ||
    arr2d[i,j - 1].IsBorderCon ||
    arr2d[i,j + 1].IsBorderCon

/// Sets every horizontal or vertical neighbor of a Pixel as being border-connected if its value is 1.
let setNeighbors i j (arr2d : Pixel [,]) =
    let setNeighbor px = if px.Value = 1 then {px with IsBorderCon = true} else px
    arr2d[i - 1,j] <- setNeighbor arr2d[i - 1,j]
    arr2d[i + 1,j] <- setNeighbor arr2d[i + 1,j]
    arr2d[i,j + 1] <- setNeighbor arr2d[i,j + 1]
    arr2d[i,j - 1] <- setNeighbor arr2d[i,j - 1]

/// Takes a 2D Pixel array and identifies and sets all border-connected Pixels.
let setBorderConnections arr2d =
    let newArr2d = Array2D.copy arr2d
    newArr2d
    |> Array2D.iteri (
        fun i j px ->
            let indexCondition = i > 0 && j > 0 && i < newArr2d.GetLength(0) - 1 && j < newArr2d.GetLength(1) - 1
            let neighborBorderConCondition () = isAnyNeighborBorderCon i j newArr2d
            if indexCondition && px.Value = 1 && neighborBorderConCondition () then
                newArr2d[i,j] <- {px with IsBorderCon = true}
                setNeighbors i j newArr2d
    )
    newArr2d

let inputWithBorderConnections = setBorderConnections inputWithBorderCheckedPixel

inputWithBorderCheckedPixel |> stringifyPxArr
inputWithBorderCheckedPixel |> deleteNonBlackPixel |> stringifyPxArr
inputWithBorderConnections |> stringifyPxArr

let inputWithoutIslands = deleteNonBlackPixel inputWithBorderConnections
inputWithoutIslands |> stringifyPxArr

let deletedPixels = 
    (inputAsPixelArr2d,inputWithoutIslands)
    ||> Array2D.map2 (
        fun px1 px2 ->
            if px1.Value = 1 && px2.Value = 0 then {px1 with Value = 1} else {px1 with Value = 0}
    )
deletedPixels |> stringifyPxArr