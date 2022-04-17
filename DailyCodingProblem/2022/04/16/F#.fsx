type Node =
    | Node of char * Node option * Node option

let testTree = Node ('a', Some (Node ('b', Some (Node ('d', None, None)), None)), (Some (Node ('c', None, None))))

let getDeepestNode root =
    let rec loop count node (list : (int * char) list) =
        match node with
        | Node (name, Some left, None) -> loop (count + 1) left list 
        | Node (name, None, Some right) -> loop (count + 1) right list
        | Node (name, Some left, Some right) -> loop (count + 1) left list @ loop (count + 1) right list
        | Node (name, None, None) -> (count, name) :: list
    loop 0 root []
    |> List.maxBy fst
    |> snd

getDeepestNode testTree