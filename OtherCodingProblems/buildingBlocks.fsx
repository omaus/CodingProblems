// Task: 
// Given a row of building blocks (BB), choose the one that has the closest distance to any of the required buildings that a block can inhibit.
// E.g. the following:
// BB1: School: true, Gym: false, Store: false
// BB2: School: false, Gym: true, Store: true
// BB3: School: false, Gym: false, Store: true
// The optimal BB would be BB2 since it has direkt access to Gym and Store and only has to go one block above to get to School.
// The number of required buildings and BBs can be ranging from `2` to `n`.
// Every required building will be listed in the respective BB with being `true` or `false`.
// taken from: https://www.youtube.com/watch?v=rw4s4M3hFfs#t=2m48s (beginning at about 2:45 as in the timestamp)

// The type representation of the problem.
type BuildingBlock = {
    Gym     : bool
    School  : bool
    Store   : bool
}

let bbs = [|
    {Gym = false; School = true; Store = false}
    {Gym = true; School = false; Store = false}
    {Gym = true; School = true; Store = false}
    {Gym = false; School = true; Store = false}
    {Gym = false; School = true; Store = true}
|]

let required = ["Gym"; "School"; "Store"]

open System
open FSharp.Reflection

/// Takes a buildingName and a BuildingBlock and returns if it is present in this BuildingBlock or not.
let returnBbFieldValue buildingName (bb : BuildingBlock) =
    let propertyInfos = FSharpType.GetRecordFields(bbs[0].GetType())
    FSharpValue.GetRecordField(bb, 
        propertyInfos |> Array.find (fun t -> t.Name = buildingName)
    ) :?> bool

/// Takes a buildingName, a buildingNo and an array of BuildingBlocks and returns the distance one has to walk from the BuildingBlock with the buildingNo to get to a building of the buildingName going downwards (increasing buildingNo).
let distanceToBottom buildingName buildingNo (bbs : BuildingBlock []) = 
    let rec loop i =
        if returnBbFieldValue buildingName bbs[buildingNo + i] then
            i
        elif (buildingNo + i) < bbs.Length - 1 then 
            loop (i + 1)
        else bbs.Length
    loop 0

/// Takes a buildingName, a buildingNo and an array of BuildingBlocks and returns the distance one has to walk from the BuildingBlock with the buildingNo to get to a building of the buildingName going upwards (decreasing buildingNo).
let distanceToTop buildingName buildingNo (bbs : BuildingBlock []) = 
    let rec loop i count =
        if returnBbFieldValue buildingName bbs[i] then 
            count
        elif i > 0 then
            loop (i - 1) (count + 1)
        else bbs.Length
    loop (bbs.Length - 1 - (bbs.Length - 1 - buildingNo)) 0

// Enhanced type for the evaluated BB
type BuildingBlockEval = {
    BuildingBlock   : BuildingBlock
    BuildingNo      : int
    ReqBuildDist    : (string * int) list
    MaxDistance     : int
}

/// Takes an array of BuildingBlocks and returns them evaluated.
let getBuildingDistanceList (bbs : BuildingBlock []) =
    bbs
    |> Array.mapi (
        fun i bb -> 
            let rbd = 
                required
                |> List.map (
                    fun rb ->
                        rb, Math.Min(distanceToBottom rb i bbs, distanceToTop rb i bbs)
                )
            let md = List.maxBy snd rbd |> snd // `List.sum` if one accounts for overall distance in general
            {
                BuildingBlock   = bb
                BuildingNo      = i
                ReqBuildDist    = rbd
                MaxDistance     = md
            }
    )

/// Takes an array of BuildingBlocks and returns the BuildingBlock with the smallest maximum distance to any required building.
let getBestBuilding bbs =
    getBuildingDistanceList bbs
    |> Array.minBy (fun bbe -> bbe.MaxDistance)

getBestBuilding bbs