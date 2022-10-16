(*
Task: Implement a binary tree that always has either none or two children.
Example tree looks like this:
       O
      / \
     /   \
    O     O
   / \   / \
  O   O O   O
 / \
O   O
   / \
  O   O
*)

// Approach 1: Discriminated Union
type BinaryTreeDU =
    | Node of BinaryTreeDU * BinaryTreeDU
    | Tip

let exampleBTDU = 
    Node (
        Node (
            Node (
                Tip,
                Node (
                    Tip,
                    Tip
                )
            ),
            Tip
        ),
        Node (
            Tip,
            Tip
        )
    )

(*
Advantages:
- DU fits very well for this as the type representation itself is very simple
- Relatively easy deconstruction with pattern matching
Disadvantages: 
- Construction can be tedious and confusing due to tuples
- Even more tupling around for values contained in Nodes and Tips
*)


// Approach 2: Record Type with option type (Some for Node, None for Tip)
type BinaryTreeRT = {
    Left    : BinaryTreeRT option
    Right   : BinaryTreeRT option
}

let exampleBTRT = {
    Left = Some {
        Left = Some {
            Left = None
            Right = Some {
                Left = None
                Right = None
            }
        }
        Right = None
    }
    Right = Some {
        Left = None
        Right = None
    }
}

(*
Advantages:
- Construction is a bit less confusing due to named fields
- Values can be realized as additional fields
- Easy access on branches of instances
Disadvantages: 
- Using option type may be annoying
    - in construction
    - in deconstruction
*)


// Approach 3: Class
type BinaryTreeClass(?left : BinaryTreeClass, ?right : BinaryTreeClass) =
    member this.Left = left
    member this.Right = right

let exampleBTClass =
    BinaryTreeClass(
        left = BinaryTreeClass(
            left = BinaryTreeClass(),
            right = BinaryTreeClass(
                left = BinaryTreeClass(),
                right = BinaryTreeClass()
            )
        ),
        right = BinaryTreeClass(
            left = BinaryTreeClass(),
            right = BinaryTreeClass()
        )
    )

(*
Advantages:
- Easiest and most clear construction due to optional parameters
- Values can be realized as additional properties
- Easy access on branches of instances
Disadvantages: 
- Deconstruction as annoying as with RT approach due to option type
*)


// Approach 4: DU & RT mixed

type BinaryTreeMixedDU =
    | Node of BinaryTreeMixedRT
    | Tip

and BinaryTreeMixedRT = {
    Left    : BinaryTreeMixedDU
    Right   : BinaryTreeMixedDU
}

let exampleBTMixed =
    Node {
        Left = Node {
            Left = Node {
                Left = Tip
                Right = Node {
                    Left = Tip
                    Right = Tip
                }
            }
            Right = Tip
        }
        Right = Node {
            Left = Tip
            Right = Tip
        }
    }

(*
Advantages:
- Combines the strengths of DU and RT in representation
Disadvantages: 
- Still more verbose than Class (but this verbosity could also be an advantage – depends on individual taste)
- Deconstruction a bit more difficult than pure DU due to two types
*)