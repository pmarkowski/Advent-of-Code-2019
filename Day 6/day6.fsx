open System
open System.IO

let inputPath = Path.Combine(__SOURCE_DIRECTORY__, "input.txt")
let testInputPath = Path.Combine(__SOURCE_DIRECTORY__, "sample-input.txt")

type OrbitMap = { Name: string; OrbitedBy: (OrbitMap list); }

let orbitStringToCouple (orbitString:string) =
    let orbitPair = orbitString.Split([|')'|])
    (orbitPair.[0], orbitPair.[1]) 

let addToOrbitMap (orbitMap:OrbitMap) (orbitPair: string * string) =
    if orbitMap.Name = (fst orbitPair) then
        {orbitMap with OrbitedBy = { Name = (snd orbitPair)::orbitMap.OrbitedBy} }
    else
        // return orbit map having called addToOrbitMap to all its children

let orbitCouplesToOrbitMap
    (orbitCouples:seq<string * string>)
    : OrbitMap =
    let initialOrbitMap = { Name = "COM"; OrbitedBy = Seq.empty }
    orbitCouples
    |> Seq.fold addToOrbitMap initialOrbitMap

let calculateChecksum (orbitMap:OrbitMap) : int =
    0

let solveProblem1 inputPath =
    File.ReadLines inputPath
    |> Seq.map orbitStringToCouple
    |> orbitCouplesToOrbitMap
    |> calculateChecksum
