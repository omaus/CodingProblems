// Task:
// Excel uses column names like A, B, C ... AA, AB, AC up to infinite. Write a function to convert the column name to its column index and vice versa. 
// taken from: https://qr.ae/pvs18n (Question 1)

open System

let toExcelLetters number =
    if int number < 1 then failwith "Only numbers > 0 can be converted to Excel letters."
    let rec loop no list =
        if no > 26. then
            loop 
                (if no % 26. = 0. then no / 26. - 0.1 else no / 26.) 
                (if no % 26. = 0. then 'Z' :: list else (no % 26. + 64. |> char) :: list)
        else
            if no % 26. = 0. then 'Z' :: list else (no % 26. + 64. |> char) :: list
    loop (float number) []
    |> String.Concat

toExcelLetters 27

let toNumber excelLetters =
    if String.forall (fun l -> int l < 65 || int l > 90) excelLetters then failwith "Only uppercase letters ranging from A to Z are allowed."
    let len = String.length excelLetters |> float
    excelLetters.ToCharArray()
    |> Array.mapi (
        fun i l -> 
            let letterNum = float l - 64.
            let exp = (float i - len |> Math.Abs) - 1.
            letterNum * 26. ** exp |> int
    )
    |> Array.sum

toNumber "AA"