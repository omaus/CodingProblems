open System

let isPrimeNumber number =
    let rec iter i boundary =
        if number % i = 0. then false
        elif i <= boundary then
            iter (i + 2.) boundary
        else true
    let boundary = Math.Sqrt(number) |> int
    match number with
    | x when x <= 1. -> false
    | 2. 
    | 3. -> true
    | x when x % 2. = 0. -> false
    | _ -> iter 3. boundary

let getFewestSteps n =
    let getSmallestPossibleFactor v = Math.Sqrt(v) |> int |> float
    let isCleanRoot n = Math.Sqrt(n) % (Math.Sqrt(n) |> int |> float) = 0.
    let rec getSmallestFactors v a b =
        if a * b < v then
            getSmallestFactors v a (b + 1.)
        elif a * b > v then
            getSmallestFactors v (a - 1.) b
        else Math.Max(a, b)
    let rec computeNextStep v list =
        if v = 1. then list
        elif v |> isPrimeNumber then 
            computeNextStep (v - 1.) (v - 1. :: list)
        else 
            if isCleanRoot v then 
                computeNextStep (Math.Sqrt(v)) (Math.Sqrt(v) :: list)
            else 
                let spf = getSmallestPossibleFactor v
                let sf = getSmallestFactors v spf 1.
                computeNextStep sf (sf :: list)
    computeNextStep n [n] 
    |> fun res -> List.length res - 1, List.rev res

getFewestSteps 10