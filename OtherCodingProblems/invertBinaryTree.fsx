// Task:
// Write a function that inverts a binary tree.

/// Union type representation of a binary tree.
type Node =
    | Node of Node * Node
    | End

let testTree = Node (Node (Node (End, End), End), Node (End, End))
let testTree2 = Node (End, Node (End, End))
let testTree3 = Node (End, Node (End, Node(End, End)))

let rec invertTree tree =
    match tree with
    | End -> tree
    | Node (left, right) -> Node (invertTree right, invertTree left)

invertTree testTree
invertTree testTree2
invertTree testTree3


/// Class type representation of a binary tree. Getter and Setter are not mandatory, actually.
type Node2(?left : Node2, ?right : Node2) =
    let mutable mutLeft = left
    let mutable mutRight = right
    member this.Left 
        with get() = mutLeft
        and set(value) = mutLeft <- value
    member this.Right 
        with get() = mutRight
        and set(value) = mutRight <- value

let testTree4 = Node2()
let testTree5 = Node2(Node2())
let testTree6 = Node2(Node2(Node2()), Node2())

let rec invertTree2 (tree : Node2) =
    match tree.Left, tree.Right with
    | None, None -> tree
    | None, Some right -> Node2(invertTree2 right)
    | Some left, None -> Node2(right = invertTree2 left)
    | Some left, Some right -> Node2(invertTree2 right, invertTree2 left)

let resTree = invertTree2 testTree6

testTree6.Left
testTree6.Right
resTree.Left
resTree.Right