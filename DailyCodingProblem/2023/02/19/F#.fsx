#r "nuget: FSharpAux"

open FSharpAux

let returnSubSeqs str : string [] =
    let chArr = String.toCharArray str
    chArr
    |> Array.mapi (
        fun i c1 ->
            chArr
            |> Array.choosei (
                fun j c2 -> 
                    if j < i then None
                    else Some (chArr[i .. j] |> System.String)
            )
    )
    |> Array.concat

returnSubSeqs "xyz"