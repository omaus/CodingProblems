let testArray = [|-4; 5; 1; 0; -3; 2|]
let testArray2 = [|5; -4; 5; 1; 0; -3; 2|]

// both wrong since they focus on looking for numbers smaller or equal than 0. This must not necessary be true as a boundary for subarrays.

// imperative approach
let getMaxSubArrayImp arr =
    let mutable storeAcc = 0
    let mutable workingAcc = 0
    let mutable overlapAcc = 0
    let mutable firstEventNo = false
    for v in arr do
        if v <= 0 then
            firstEventNo <- true
            if workingAcc > storeAcc then
                storeAcc <- 0
            else 
                workingAcc <- storeAcc
                storeAcc <- 0
        else storeAcc <- storeAcc + v
        if firstEventNo |> not then 
            overlapAcc <- overlapAcc + v
    overlapAcc <- storeAcc + overlapAcc
    if workingAcc > overlapAcc then workingAcc else overlapAcc

getMaxSubArrayImp testArray
getMaxSubArrayImp testArray2

// functional approach
let getMaxSubArrayFun (arr : int []) =
    let rec loop i storeAcc workingAcc overlapAcc firstEventNo =
        if i < arr.Length then
            if arr[i] <= 0 then
                if workingAcc > storeAcc then
                    loop (i + 1) workingAcc 0 overlapAcc true
                else 
                    loop (i + 1) storeAcc 0 overlapAcc true
            else 
                let ola = if firstEventNo |> not then overlapAcc + arr[i] else overlapAcc
                loop (i + 1) storeAcc (workingAcc + arr[i]) ola firstEventNo
        else 
            if storeAcc > (overlapAcc + workingAcc) then 
                storeAcc
            else (overlapAcc + workingAcc)
    loop 0 0 0 0 false

getMaxSubArrayFun testArray
getMaxSubArrayFun testArray2