open System.Collections.Generic

type DataStructure() =

    let ds1 = Dictionary<obj,int>()
    let ds2 = Dictionary<int,obj list>()
    Tab

    member this.plus key = 
        try 
            ds1.Add(key,1)
            try ds2.Item 1 <- key :: ds2.Item 1
            with :? KeyNotFoundException -> ds2.Add(1, [key])
        with :? System.ArgumentException -> 
            let ov = ds1.Item key
            let nv = ds1.Item key + 1
            ds1.Item key <- nv
            try 
                ds2.Item nv <- key :: ds2.Item nv
                // I have no idea how to remove the item at that list without breaking the O(1) requirement...
                ds2.Item ov <- List.remove key ds.Item ov
            with :? KeyNotFoundException -> ds2.Add(nv, [key])