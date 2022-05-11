// Task:
// Write a function to compute the nth Fibonacci number.
// taken from: https://sites.google.com/site/steveyegge2/five-essential-phone-screen-questions

let getFibonacci n =
    if n = 0 then 0 
    else
        let rec loop i currNum formerNum =
            if i < n then loop (i + 1) (currNum + formerNum) currNum
            else currNum + formerNum
        loop 2 1 0

let getFibonacci' n =
    match n with
    | 0 -> 0
    | 1 -> 1
    | _ ->
        let mutable currNum = 1
        let mutable formerNum = 0
        let mutable i = 1
        while i < n do
            let tmp = currNum
            currNum <- formerNum + currNum
            formerNum <- tmp
            i <- i + 1
        currNum

getFibonacci 7
getFibonacci' 7