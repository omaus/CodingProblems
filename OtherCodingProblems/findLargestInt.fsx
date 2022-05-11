// Task:
// Find the largest int value in an int array.
// taken from: https://sites.google.com/site/steveyegge2/five-essential-phone-screen-questions

let getLargestInt intArr = Array.max intArr

let getLargestInt' (intArr : int []) =
    let l = intArr.Length
    let rec loop i max =
        if i < l then
            if intArr[i] > max then loop (i + 1) intArr[i]
            else loop (i + 1) max
        else max
    loop 0 (Array.head intArr)

let getLargestInt'' intArr =
    let mutable max = Array.head intArr
    for n in intArr do
        if n > max then max <- n
    max

let testArr = [|82; 84; 52; 81; 0; 35; 49; 93; 53; 65; 59; 60; 39; 34; 23; 56; 20; 53; 92; 68; 21; 95; 100; 25; 54; 35; 40; 90; 84; 12; 35|]

getLargestInt testArr
getLargestInt' testArr
getLargestInt'' testArr