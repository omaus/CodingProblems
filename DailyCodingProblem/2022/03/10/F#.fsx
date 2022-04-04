type MyStack<'a when 'a : comparison>() =
    let mutable stack = Seq.empty
    member inline this.Push(v : 'a) = stack <- Seq.insertAt 0 v stack
    member inline this.Pop() = 
        let i0 = Seq.head stack
        stack <- Seq.removeAt 0 stack
        i0
    member inline this.Max() =
        let mutable tmp = Seq.head stack
        Seq.iter (fun v -> if v > tmp then tmp <- v) stack
        tmp

let myStack = MyStack<string>()
myStack.Push("hans")
myStack.Max()
myStack.Push("guenther")
let myStack2 = MyStack<int>()
myStack2.Push(1)
myStack2.Push(111)
myStack2.Push(21)
myStack2.Max()
myStack2.Pop() 