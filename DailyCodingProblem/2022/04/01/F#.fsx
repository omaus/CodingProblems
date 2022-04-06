#r "nuget: FSharpAux"

open FSharpAux

module Array2D =
    let iteriSpiral action (arr2d: 'T [,]) =
        let rec spiral fstRow lstRow fstCol lstCol =
            let checkRows = lstRow - fstRow
            let checkCols = lstCol - fstCol
            if checkCols > 0 then for i = fstCol     to     lstCol     do action fstRow i arr2d[fstRow,i]
            if checkRows > 0 then for i = fstRow + 1 to     lstRow     do action i lstCol arr2d[i,lstCol]
            if checkCols > 0 then for i = lstCol - 1 downto fstCol     do action lstRow i arr2d[lstRow,i]
            if checkRows > 0 then for i = lstRow - 1 downto fstRow + 1 do action i fstCol arr2d[i,fstCol]
            if checkRows > 0 || checkCols > 0 then spiral (fstRow + 1) (lstRow - 1) (fstCol + 1) (lstCol - 1)
        spiral 0 (arr2d.GetLength(0) - 1) 0 (arr2d.GetLength(1) - 1)

    let iterSpiral action (arr2d : 'T [,]) = iteriSpiral (fun _ _ v -> action v) arr2d


let testMatrix = array2D [
    [ 1;  2;  3;  4;  5]
    [ 6;  7;  8;  9; 10]
    [11; 12; 13; 14; 15]
    [16; 17; 18; 19; 20]
]

let testMatrix2 = Array2D.transpose testMatrix

Array2D.iterSpiral (fun v -> printfn $"{v}") testMatrix
Array2D.iterSpiral (fun v -> printfn $"{v}") testMatrix2