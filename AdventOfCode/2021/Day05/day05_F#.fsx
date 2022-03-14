// ______
// PART 1
// ‾‾‾‾‾‾

#r "nuget: FSharpAux"

open FSharpAux

let example = "0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2"

type VentCoords = {
    x1  : int
    x2  : int
    y1  : int
    y2  : int
}

let parseInput input =
    String.split '\n' input
    |> Array.map (
        String.splitS " -> " 
        >> fun coordPair -> 
            let x1y1 = String.split ',' coordPair.[0]
            let x2y2 = String.split ',' coordPair.[1]
            {x1 = int x1y1.[0]; x2 = int x2y2.[0]; y1 = int x1y1.[1]; y2 = int x2y2.[1]}
    )

let ventCoordsExample = parseInput example

let maxXexample = ventCoordsExample |> Array.fold (fun acc t -> if acc > System.Math.Max(t.x1, t.x2) then acc else System.Math.Max(t.x1, t.x2)) 0
let maxYexample = ventCoordsExample |> Array.fold (fun acc t -> if acc > System.Math.Max(t.y1, t.y2) then acc else System.Math.Max(t.y1, t.y2)) 0

let ventMapExample = Array2D.init (maxXexample + 1) (maxYexample + 1) (fun _ _ -> 0)

let updateMapByCoords ventMap ventCoords =
    ventMap
    |> Array2D.mapi (
        fun yi xi v -> 
            if xi >= ventCoords.x1 && xi <= ventCoords.x2 && yi >= ventCoords.y1 && yi <= ventCoords.y2 then v + 1
            else v
    )

updateMapByCoords ventCoordsExample.[0] ventMapExample

