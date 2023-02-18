let testArr = [|-1; 5; 13; 8; 2; 3; 3; 1|]

//let median (arr : 'a [] when 'a : (static member (+) : 'a -> 'a -> 'a)) =
let inline median (arr : 'a []) =
    let l = arr.Length
    let sArr = Array.sort arr
    if l % 2 = 1 then sArr[(l - 1) / 2]
    else (sArr[l / 2 - 1] + sArr[l / 2]) / (LanguagePrimitives.GenericOne + LanguagePrimitives.GenericOne)

[|1; 3; 0|] |> median
median [|1; 2|]

// the easy way with in-built function `Array.windowed`:
let inline getWindowMedian k arr = 
    Array.windowed k arr
    |> Array.map median

getWindowMedian 3 testArr

// the more difficult approach with building my own `windowed` function:
let arrayWindowed size (arr : 'T []) = [|for i = 0 to arr.Length - size do arr[i .. i + size - 1]|]

arrayWindowed 3 testArr

let inline getWindowMedian' k arr =
    arrayWindowed k arr
    |> Array.map median

getWindowMedian' 3 testArr