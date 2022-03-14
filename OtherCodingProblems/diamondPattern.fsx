// Task: 
// Write a function that prints a diamond pattern, given a number of rows.
// It should look like this: (input: 3 rows; 9 rows)
//  * 
// ***
//  *
// 
//     *    
//    ***   
//   *****  
//  ******* 
// *********
//  ******* 
//   *****  
//    ***   
//     *    

/// Takes a number of rows and prints a diamond according to that number into the console.
let printDiamond noOfRows = // mutable way
    if noOfRows % 2 = 0 then failwith "ERROR: Only odd number of rows are allowed."
    let arr2d = Array2D.init noOfRows noOfRows (fun _ _ -> " ")
    let mutable rowCounter = 0
    let mutable middleDistance = 0
    let mutable middleCheck = false
    let middleRow = noOfRows / 2
    while rowCounter < noOfRows do
        let leftBorder, rightBorder = middleRow - middleDistance, middleRow + middleDistance
        let mutable i = 0
        while i < noOfRows do
            if i >= leftBorder && i <= rightBorder then 
                Array2D.set arr2d rowCounter i (*"┼"*) "*"
            i <- i + 1
        if middleCheck then
            middleDistance <- middleDistance - 1
        else middleDistance <- middleDistance + 1
        if middleDistance = middleRow then middleCheck <- true
        rowCounter <- rowCounter + 1
    arr2d
    |> Array2D.iteri (
        fun i j v -> 
            if j < noOfRows - 1 then printf $"{v}"
            else printfn $"{v}"
    )

printDiamond 5


/// Takes a number of rows and prints a diamond according to that number into the console.
let printDiamond' noOfRows = // immutable way
    if noOfRows % 2 = 0 then failwith "ERROR: Only odd number of rows are allowed."
    let halfRows = noOfRows / 2
    Array2D.init noOfRows noOfRows (
        fun i j -> 
            let cond1 = j < System.Math.Abs(halfRows - i)
            let cond2 = j > System.Math.Abs(halfRows + i)
            let cond3 = j + i >= noOfRows + halfRows
            if cond1 || cond2 || cond3 then " "
            else "*" (*"┼"*)
    )
    |> Array2D.iteri (
        fun i j v -> 
            if j = noOfRows - 1 then printfn $"{v}"
            else printf $"{v}"
    )

printDiamond' 5