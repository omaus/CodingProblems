/// Binary Tree representation type.
type BinaryTree = 
    | Node of string * BinaryTree * BinaryTree
    | Tip of string

let testTree =
    Node (
        "1",
        Node (
            "2",
            Tip "4",
            Tip "5"
        ),
        Node (
            "3",
            Tip "6",
            Tip "7"
        )
    )

let writeBoustrophedon bt =
    let rec loop li cbt order =
        match cbt with
        | Tip v -> v :: li
        | Node (v,l,r) -> loop (v :: li) l @ loop li r
    loop [] bt

writeBoustrophedon testTree

type BinaryTreeRec = {
    Left    : BinaryTreeRec option
    Right   : BinaryTreeRec option
    Value   : string
}

let testTreeRec = {
    Value = "1"
    Left = Some {
        Value = "2"
        Left = Some {
            Value = "4"
            Left = None
            Right = None
        }
        Right = Some {
            Value = "5"
            Left = None
            Right = None
        }
    }
    Right = Some {
        Value = "3"
        Left = Some {
            Value = "6"
            Left = None
            Right = None
        }
        Right = Some {
            Value = "7"
            Left = None
            Right = None
        }
    }
}