// Task:
// A man buys a young calf.
// After 3 years, the calf calves (gives birth to a calf) for the first time and afterwards once per year.
// Every newborn calf itself calves every year once 3 years after it was born (like the first calf).
// Biology aside, all calves behave the same, are immortal, and there's no bulls required.
// How many calves are living on the man's farm 18 years after he bought the very first calf?

// functionally
let rec cowLife cows startYear endYear =
    let rec elongCows oldCowList newCowList =
        match oldCowList with
        | h :: t -> 
            if h >= 3 then 
                elongCows t (0 :: h :: newCowList)
            else elongCows t (h :: newCowList)
        | [] -> newCowList
    if startYear <= endYear then
        let olderCows = cows |> List.map ((+) 1)
        cowLife (elongCows olderCows []) (startYear + 1) endYear
    else cows.Length

cowLife [0] 1 18

// imperatively
let cowLife2 cows startYear endYear =
    let mutable cowList = cows
    for i = startYear to endYear do
        cowList <- (cowList |> List.map ((+) 1))
        cowList
        |> List.iter (
            fun c -> 
                if c >= 3 then 
                    cowList <- 0 :: cowList
        )
    cowList.Length

cowLife2 [0] 1 18