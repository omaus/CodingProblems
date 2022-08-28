#r "nuget: FSharpAux"

open FSharpAux

let testString = "abracadabra"
let testPattern = "abr"

let getOccurences (pattern : string) (str : string) =
    let rec loop i containerList =
        if i < str.Length - pattern.Length + 1 then
            let set =
                str[i ..]
                |> String.take pattern.Length
            let newContainerList =
                if set = pattern then i :: containerList
                else containerList
            loop (i + 1) newContainerList
        else containerList
    loop 0 []
    |> List.rev

getOccurences testPattern testString

getOccurences "aba" "abracadababa"