// Task:
// Write a function that calculates the gcd (greatest common divisor) of 2 given numbers.

open System

let getGcd x y = x

// Euclid's algorithm:
let getGcdEuclid (x : int) y =
    if x = 0 || y = 0 then 0
    else 
        let rec loop biggerNo smallerNo : int =
            if biggerNo = smallerNo then biggerNo
            else 
                let newNo = biggerNo - smallerNo
                loop (Math.Max(newNo, smallerNo)) (Math.Min(newNo, smallerNo))
        loop (Math.Max(x, y)) (Math.Min(x, y))

getGcdEuclid 48 18
getGcdEuclid 48 46
getGcdEuclid 00 00
getGcdEuclid 00 10

// Euclidian algorithm: (roughly the same but with remainder)
let getGcdEuclidian (x : int) y =
    if x = 0 || y = 0 then 0
    else 
        let rec loop biggerNo smallerNo : int =
            if biggerNo = 0 then smallerNo
            elif smallerNo = 0 then biggerNo
            else
                let rem = biggerNo % smallerNo
                loop (Math.Max(rem, smallerNo)) (Math.Min (rem, smallerNo))
        loop (Math.Max(x, y)) (Math.Min(x, y))

getGcdEuclidian 48 18
getGcdEuclidian 48 46
getGcdEuclidian 00 00
getGcdEuclidian 00 10

// Lehmer's algorithm:
let getGcdLehmer x y = x

// Binary algorithm:
let getGcdBinary x y = x

// using lcm:
let getGcdLcm x y = x

// prime factorization:
let getGcdPf x y = x