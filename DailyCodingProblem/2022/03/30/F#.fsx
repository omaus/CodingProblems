#r "nuget: FSharpAux"

open FSharpAux

let testMatrix = array2D [
    ['F'; 'A'; 'C'; 'I']
    ['O'; 'B'; 'Q'; 'P']
    ['A'; 'N'; 'O'; 'B']
    ['M'; 'A'; 'S'; 'S']
]

let findWordInMatrix word (matrix : char [,]) =
    let nRows = matrix.GetLength(0)
    let nCols = matrix.GetLength(1)
    let rec checkRows i =
        if String.contains word (System.String matrix[i,0 ..]) then true 
        elif i < nRows - 1 then checkRows (i + 1)
        else false
    let rec checkCols j =
        if String.contains word (System.String matrix[0 ..,j]) then true 
        elif j < nCols - 1 then checkCols (j + 1)
        else false
    if not (checkRows 0) then checkCols 0 else true

findWordInMatrix "FOAM" testMatrix
findWordInMatrix "MASS" testMatrix
findWordInMatrix "ANBO" testMatrix