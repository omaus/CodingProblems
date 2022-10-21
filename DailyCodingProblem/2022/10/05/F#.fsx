open type System.Math

// This is my implementation which is not as simple as it COULD be but it works though
/// Takes two integers and returns the bigger one.
let returnBigger (a : int) b =
    let c = Abs(a - b) / (a - b)
    let d = Abs(b - a) / (b - a)
    (d * b + Abs(d) * b) / 2 + (c * a + Abs(c) * a) / 2

returnBigger 5 2
returnBigger 2 5
returnBigger 10 0
returnBigger 9 -50

// The mathematically simplest (and thus most elegant) implementation, found on StackOverflow
/// Takes two integers and returns the bigger one.
let returnBigger' (a : int) b = ((a + b) + Abs(a - b)) / 2

returnBigger' 5 2
returnBigger' 2 5
returnBigger' 10 0
returnBigger' 9 -50