// Task:
// Write a function to reverse a string.
// taken from: https://sites.google.com/site/steveyegge2/five-essential-phone-screen-questions

let reverseString (str : string) = 
    let iMax = str.Length - 1
    let newStr : char [] = Array.zeroCreate str.Length
    for i = 0 to iMax do newStr[i] <- str[iMax - i]
    System.String newStr

let reverseString' (str : string) =
    let l = str.Length
    String.mapi (fun i c -> str[l - i - 1]) str

reverseString "Hello, World!"
reverseString' "Hello, World!"