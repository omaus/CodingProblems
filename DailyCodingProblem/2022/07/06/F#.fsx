// imperative
let getFstRecurrChar (input : string) =
    let chArr = input.ToCharArray()
    let l = chArr.Length
    let mutable res = null
    let mutable i = 0
    while i <= l - 2 && res = null do
        let mutable j = i + 1
        while j <= l - 1 && res = null do
            if chArr[i] = chArr[j] then res <- chArr[i] |> string
            j <- j + 1
        i <- i + 1
    res

getFstRecurrChar "aasdgf"

// functional
let getFstRecurrChar' (input : string) =
    let chArr = input.ToCharArray()
    let l = chArr.Length
    let rec loop i j previousChar =
        match i,j with
        | (x,y) when x <= l - 2 && y <= l - 1 -> 
            if previousChar = chArr[j] then previousChar |> string
            else loop i (j + 1) previousChar
        | (x,y) when x <= l - 2 && y > l - 1 -> loop (i + 1) (i + 2) chArr[i + 1]
        | _ -> null
    loop 0 1 chArr[0]

getFstRecurrChar' "abcd"