// Task:
// Write a function that takes a parameter `n` and increases a number, beginning at 1 and ending at `n`. 
// The function shall print the number into the console. 
// If the number can be divided by 3 it shall be replaced with "Fizz", it it can be divided by 5, with "Buzz", and if it can be divided by both, with "FizzBuzz".

let printFizzBuzz n =
    for i = 1 to n do
        match i % 3, i % 5 with
        | 0,0   -> "FizzBuzz"
        | 0,_   -> "Fizz"
        | _,0   -> "Buzz"
        | _,_   -> string i
        |> System.Console.WriteLine

printFizzBuzz 100