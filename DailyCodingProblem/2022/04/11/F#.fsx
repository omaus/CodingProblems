let testArray = [|0; 8; 4; 12; 2; 10; 6; 14; 1; 9; 5; 13; 3; 11; 7; 15|]

let toLongestSubSeq (input : int []) =
    let rec loop i currNum newNum (listOfLists : int list list)=
        if i < input.Length - 1 then
            if newNum <= listOfLists.Head.Head then
                loop (i + 1) currNum input[i + 1] ([newNum] :: listOfLists)
                @
                loop (i + 1) newNum input[i + 1] listOfLists
            else 
                loop (i + 1) newNum input[i + 1] ((newNum :: listOfLists.Head) :: listOfLists.Tail)
                @
                loop (i + 1) currNum input[i + 1] listOfLists
        else
            if newNum <= listOfLists.Head.Head then
                ([newNum]) :: listOfLists
            else (newNum :: listOfLists.Head) :: listOfLists.Tail
    loop 1 input[0] input[1] [[input[0]]]
    |> List.map List.rev
    |> List.rev


toLongestSubSeq testArray
|> List.maxBy List.length
toLongestSubSeq [|0; 2; 6; 4; 8; 15; 10|]
|> List.maxBy List.length