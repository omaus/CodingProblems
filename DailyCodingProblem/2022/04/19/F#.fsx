type BinaryTree(name : string, ?left : BinaryTree, ?right : BinaryTree) =
    let mutable left = left
    let mutable right = right
    let mutable name = name
    member this.Left 
        with get() = left
        and set(value) = left <- value
    member this.Right 
        with get() = right
        and set(value) = right <- value
    member this.Name 
        with get() = name
        and set(value) = name <- value

let invertBinaryTreeInPlace tree =
    let rec loop (node : BinaryTree) : unit =
        match node.Left, node.Right with
        | Some l, Some r -> 
            node.Left <- Some r
            loop r
            node.Right <- Some l
            loop l
        | Some l, None -> 
            node.Left <- None
            node.Right <- Some l
            loop l
        | None, Some r -> 
            node.Left <- Some r
            loop r
            node.Right <- None
        | None, None -> ()
    loop tree
    tree

let testTree = BinaryTree("a", left = BinaryTree("b", left = BinaryTree("d"), right = BinaryTree("e")), right = BinaryTree("c", left = BinaryTree("f")))
testTree.Right.Value.Left.Value.Name

invertBinaryTreeInPlace testTree
testTree.Right.Value.Left.Value.Name