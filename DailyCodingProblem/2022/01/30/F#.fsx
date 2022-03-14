let getLowestNonExistingInt arr =
    let rec loop n arr2 =
        let check = arr2 |> Array.exists (fun v -> v = n)
        if check then loop (n + 1) arr2
        else n
    loop 1 arr

getLowestNonExistingInt [|3; 4; -1; 1|]
getLowestNonExistingInt [|1; 2; 0|]

let getLowestNonExistingInt' arr =
    let mutable n = 1
    while Array.exists (fun v -> v = n) arr do
        n <- n + 1
    n

getLowestNonExistingInt' [|3; 4; -1; 1|]
getLowestNonExistingInt' [|1; 2; 0|]