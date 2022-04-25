// functional recursive approach
let divide x y =
    if x < 1 || y < 1 then failwith "Only positive integers are allowed."
    let rec loop raisedDivisor acc =
        if raisedDivisor <= x then loop (raisedDivisor + y) (acc + 1)
        else acc
    loop y 0

divide 10 2
divide 10 5
divide 10 8
divide 10 10
divide 10 11

// imperative approach
let divide' x y =
    if x < 1 || y < 1 then failwith "Only positive integers are allowed."
    let mutable acc = 0
    let mutable raisedDivisor = y
    while raisedDivisor <= x do
        raisedDivisor <- raisedDivisor + y
        acc <- acc + 1
    acc

divide' 10 2
divide' 10 5
divide' 10 8
divide' 10 10
divide' 10 11