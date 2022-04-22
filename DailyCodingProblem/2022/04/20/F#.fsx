let testMatrix = array2D [
    [1; 0; 0; 0; 0]
    [0; 0; 1; 1; 0]
    [0; 1; 1; 0; 0]
    [0; 0; 0; 0; 0]
    [1; 1; 0; 0; 1]
    [1; 1; 0; 0; 1]
]

let checkPosNeighb arr2d checkedArr2d xPos yPos =
    arr2d[xPos,yPos] = 1 &&
    match xPos, yPos with
    | 0,0 -> 
        checkedArr2d[xPos + 1, yPos]

let countIslands (arr2d : int [,]) =
    let width = arr2d.GetLength 0
    let height = arr2d.GetLength 1
    let checkeds = Array2D.create width height false
    let rec loop xPos yPos count =
        if yPos < height - 1 then
            if xPos < width - 1 then
                let cond = 
                    
                    
