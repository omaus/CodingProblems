// This is acutally a funny coding problem since it is, in fact, a mathematical problem that must be figured out while the coding implementation of it is rather easy.
// The following are some ugly approaches that use mutability:

let rnd1 = System.Random()
let rnd2 = System.Random()

let fivesix = 
    let mutable count = 0
    for i = 1 to 1000000000 do
        if rnd1.Next(1,7) = 5 && rnd2.Next(1,7) = 6 then count <- count + 1
    count

let fivefive =
    let mutable count = 0
    for i = 1 to 1000000000 do
        if rnd1.Next(1,7) = 5 && rnd2.Next(1,7) = 5 then count <- count + 1
    count

fivesix - fivefive


let fstGame () =
    let die = System.Random()
    let mutable count = 1
    let mutable five = false
    let mutable six = false
    while (not five) && (not six) do
        match five, six with
        | true, false -> 
            if die.Next(1,7) = 6 then 
                six <- true 
            elif die.Next(1,7) = 5 then
                five <- true
            else 
                five <- false
                count <- count + 1
        | false, false -> 
            if die.Next(1,7) = 5 then 
                five <- true
            else 
                five <- false
                count <- count + 1
        | _ -> failwith "shouldn't happen"
    count

let sndGame () = 
    let die = System.Random()
    let mutable count = 1
    let mutable five1 = false
    let mutable five2 = false
    while (not five1) && (not five2) do
        match five1, five2 with
        | true, false -> 
            if die.Next(1,7) = 5 then 
                five2 <- true 
            else 
                five1 <- false
                count <- count + 1
        | false, false -> 
            if die.Next(1,7) = 5 then 
                five1 <- true
            else 
                five1 <- false
                count <- count + 1
        | _ -> failwith "shouldn't happen"
    count

let fgResults = [|for i = 1 to 10000000 do fstGame ()|] |> Array.sum
let sgResults = [|for i = 1 to 10000000 do sndGame ()|] |> Array.sum

fgResults - sgResults