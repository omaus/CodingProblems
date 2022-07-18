// Task:
// Write a function that calculates the lcm (lest common multiple) of 2 given numbers.

open System

// "Brute-force" method with numerical series of both numbers.
let getLcdNS (x : int) y = 
    let biggerNo = Math.Max(x, y)
    let smallerNo = Math.Min(x, y)
    let smallerNoArr = Array.init biggerNo (fun i -> (i + 1) * smallerNo)
    let rec loop i =
        let nextBiggerNo = biggerNo * i
        if Array.contains nextBiggerNo smallerNoArr then nextBiggerNo
        else loop (i + 1)
    loop 1

getLcdNS 405 1350

// using gcd
let getLcdGcd x y = x

// prime factorization:
let getLcdPf x y = x

// table method:
let getLcdTable x y = y

// iterating method:
let getLcdIter x y =
    let rec loop formerX formerY =
        if formerX = formerY then formerX
        elif formerX > formerY then 
            loop formerX (formerY + y)
        else
            loop (formerX + x) formerY
    loop x y

getLcdIter 405 1350