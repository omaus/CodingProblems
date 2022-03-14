let getSummingUpNumbers nLi k =
    nLi
    |> List.exists (
        fun v1 ->
            nLi
            |> List.exists (
                fun v2 ->
                    v1 + v2 = k
            ) 
    )

getSummingUpNumbers [10; 15; 3; 7] 17

let getSummingUpNumbers' (nLi : int list) k =
    let mutable check = false
    let mutable i1 = -1
    let mutable i2 = -1
    let liL = nLi.Length
    while check = false && i1 < liL - 1 do
        i1 <- i1 + 1
        while check = false && i2 < liL - 1 do
            i2 <- i2 + 1
            if nLi.[i1] + nLi.[i2] = k then check <- true
    check

getSummingUpNumbers' [10; 15; 3; 7] 17