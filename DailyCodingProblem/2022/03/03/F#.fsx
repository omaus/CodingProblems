type TreeNode =
    | Node of TreeNode * TreeNode
    | End

let testTree = Node (End, Node (Node (Node (End, Node (End, End)), End), Node (End, Node (Node (Node (Node (End, Node (End, End)), End), Node (End, End)), Node (End, End)))))

let countTreeNodes binaryTree =
    let rec loop i node countList =
        match node with
        | End -> i :: countList
        | Node (treeNode1, treeNode2) -> 
            loop (i + 1) treeNode1 countList
            |> loop (i + 1) treeNode2
    loop 1 binaryTree [] |> List.rev

let getSecondLongestTreeNodes binaryTree = 
    let ctn = countTreeNodes binaryTree
    let longestTreeNodes = List.max ctn
    ctn
    |> List.fold (
        fun acc c -> 
            if c = longestTreeNodes then acc
            else 
                match acc with
                | [] -> c :: acc
                | _ ->
                    let max = List.max acc
                    if c < max then acc
                    elif c = max then c :: acc
                    else c :: []
    ) []

getSecondLongestTreeNodes testTree

let testTree2 = Node (End, Node (Node (End, End), Node (Node (End, End), End)))

countTreeNodes testTree2
getSecondLongestTreeNodes testTree2