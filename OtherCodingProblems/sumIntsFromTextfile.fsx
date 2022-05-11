// Task:
// Write a function that sums up integers from a text file, one int per line.
// taken from: https://sites.google.com/site/steveyegge2/five-essential-phone-screen-questions

open System.IO

let sumUpIntsFromTextfile path =
    let text = File.ReadAllLines path
    Array.sumBy int text

let testPath = Path.Combine(__SOURCE_DIRECTORY__, "testFile.txt")

sumUpIntsFromTextfile testPath