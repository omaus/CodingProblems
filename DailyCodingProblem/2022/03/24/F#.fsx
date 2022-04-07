#r "nuget: FSharpAux"

open FSharpAux

let testString = "the quick brown fox jumps over the lazy dog"

let splitByMaxLength s k = // recursive, pure immutability
    let asWords = String.toWords s |> List.ofSeq
    let abort = asWords |> List.forall (fun w -> w.Length <= k) |> not
    if abort then None // instead of returning null, None is returned; result is therefore of option type
    else 
        let rec loop (remainingWords : string list) (creatingWord : string) (listOfWords : string list) =
            match remainingWords with
            | [] -> creatingWord :: listOfWords
            | h :: t -> 
                if h.Length + creatingWord.Length + 1 <= k then
                    loop t $"{creatingWord} {h}" listOfWords
                else loop t h (creatingWord :: listOfWords)
        loop asWords.Tail asWords.Head [] 
        |> List.rev
        |> Some

splitByMaxLength testString 10

let splitByMaxLength' s k = // imperative, partly mutable
    let asWords = String.toWords s |> List.ofSeq
    let abort = asWords |> List.forall (fun w -> w.Length <= k) |> not
    if abort then None
    else 
        let mutable currWord = asWords.Head
        [|
            for word in asWords do
                if word = currWord then ()
                elif currWord.Length + word.Length + 1 > k then
                    yield currWord
                    currWord <- word
                else currWord <- $"{currWord} {word}"
            yield currWord
        |]
        |> Some

splitByMaxLength' testString 10