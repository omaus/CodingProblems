let pow(x, y) : int = // solution: integer factorization (dt. Primfaktorzerlegung)
    let rec getPrimeFactors restN divider list = 
        if restN % divider = 0 then
            if restN / divider = 1 then
                restN :: list
            else getPrimeFactors (restN / divider) divider (divider :: list)
        else getPrimeFactors restN (divider + 1) list
    let primeFactorList = getPrimeFactors y 2 [] |> List.rev
    List.fold (fun baseNo exp -> baseNo ** float exp) (float x) primeFactorList |> int

pow(2, 30)

// naive method of repeated multiplication
let powNaive(x, y) =
    match y with
    | 0 -> 1
    | a when a > 0 ->
        let mutable res = x
        for _ = 2 to y do res <- res * x
        res
    | _ -> failwith "Inproper base number given."

powNaive(2, 30)