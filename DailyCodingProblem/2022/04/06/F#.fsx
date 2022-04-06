// the example implicates that numbers with more than 1 digit are not allowed
let getPerfectNumber n =
    if n > 9 then failwith "Cannot be used for numbers with more than 1 digit."
    let missing = 10 - n
    n * 10 + missing

getPerfectNumber 1
getPerfectNumber 2

// if the function shall be usable for every possible number
let getPerfectNumber' n =
    let chArr = (string n).ToCharArray()
    let digitSum =
        Array.map (fun c -> int c - 48) chArr
        |> Array.sum
    if digitSum > 10 then failwith "Cannot be used for numbers whose digit sum is already above 10."
    if digitSum = 10 then n
    else
        Array.append chArr [|10 - digitSum + 48 |> char|]
        |> System.String
        |> int

getPerfectNumber' 1
getPerfectNumber' 2
getPerfectNumber' 16
getPerfectNumber' 29