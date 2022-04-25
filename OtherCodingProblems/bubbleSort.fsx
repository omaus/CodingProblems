// Task:
// Implement the Bubble Sort algorithm.

let bubbleSortInPlace (arr : 'a []) =
    let mutable check = true
    let mutable swapNeeded = false
    let mutable lastUncheckedPos = arr.Length - 2
    while check do
        if lastUncheckedPos = 0 then check <- false
        for i = 0 to lastUncheckedPos do
            let cond = arr[i] > arr[i + 1]
            if cond then
                let tmp = arr[i]
                arr[i] <- arr[i + 1]
                arr[i + 1] <- tmp
                swapNeeded <- true
            if i = lastUncheckedPos && not cond then lastUncheckedPos <- lastUncheckedPos - 1
        if not swapNeeded then check <- false
    arr

bubbleSortInPlace [|10; 5; 6; 9; 2|]

#time
bubbleSortInPlace [|1 .. 10000|]                // Bubble Sort is fast in optimal scenario...
bubbleSortInPlace ([|1 .. 10000|] |> Array.rev) // ... but slow in worst-case scenario

let bubbleSortInPlace' (arr : 'a []) =
    let rec loop i lastUncheckPos swapNeeded =
        if i <= lastUncheckPos then
            let cond = arr[i] > arr[i + 1]
            if cond then
                let tmp = arr[i]
                arr[i] <- arr[i + 1]
                arr[i + 1] <- tmp
                loop (i + 1) lastUncheckPos true
            else loop (i + 1) lastUncheckPos swapNeeded
        elif lastUncheckPos = 0 then arr
        elif swapNeeded then loop 0 (lastUncheckPos - 1) false
        else arr
    loop 0 (arr.Length - 2) false

bubbleSortInPlace' [|10; 5; 6; 9; 2|]
bubbleSortInPlace' [|1 .. 10000|]
bubbleSortInPlace' ([|1 .. 10000|] |> Array.rev)

let bubbleSort arr =
    let newArr = Array.copy arr
    bubbleSortInPlace newArr

let testArr = [|10; 5; 6; 9; 2|]

bubbleSort testArr

let bubbleSort' arr =
    let newArr = Array.copy arr
    bubbleSortInPlace' newArr

bubbleSort' testArr