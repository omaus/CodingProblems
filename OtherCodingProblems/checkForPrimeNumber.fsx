// Task:
// Write a function that checks if a given number is a prime number.

// functional approach
let isPrime n =
    let rec loop i =
        match i <= n / 2, n % i = 0 with
        | true, true -> false
        | true, false -> loop (i + 1)
        | _ -> true
    loop 2

// imperative approach
let isPrime' n =
    let mutable check = false
    let mutable i = 2
    while check |> not && i <= n / 2 do
        check <- n % i = 0
        i <- i + 1
    not check

[1 .. 20] |> List.map (fun n -> n, isPrime n)
[1 .. 20] |> List.map (fun n -> n, isPrime' n)