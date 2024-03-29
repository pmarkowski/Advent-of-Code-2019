open System
open System.IO

let inputPath = Path.Combine(__SOURCE_DIRECTORY__, "input.txt")
let testInputPath = Path.Combine(__SOURCE_DIRECTORY__, "sample-input.txt")
let test2InputPath = Path.Combine(__SOURCE_DIRECTORY__, "sample-input2.txt")

type OrbitMap = { Name: string; OrbitedBy: (OrbitMap list); }

let orbitStringToCouple (orbitString:string) =
    let orbitPair = orbitString.Split([|')'|])
    (orbitPair.[0], orbitPair.[1]) 

let rec addToOrbitMap (orbitMap:OrbitMap) (orbitPair: string * string) =
    if orbitMap.Name = (fst orbitPair) then
        {orbitMap with OrbitedBy = { Name = (snd orbitPair); OrbitedBy = []; }::orbitMap.OrbitedBy}
    else
        // return orbit map having called addToOrbitMap to all its children
        {orbitMap with OrbitedBy = List.map (fun orbitMap -> addToOrbitMap orbitMap orbitPair) orbitMap.OrbitedBy}

let orbitCouplesToOrbitMap
    (orbitCouples:seq<string * string>)
    : OrbitMap =
    
    let rec orbitCouplesToOrbitMap orbitMap orbitCouples =
        let orbitsToAdd = Seq.where (fun orbitCouple -> (fst orbitCouple) = orbitMap.Name) orbitCouples
        if Seq.isEmpty orbitsToAdd then
            orbitMap
        else
            let newOrbitMap = orbitsToAdd |> Seq.fold addToOrbitMap orbitMap
            { newOrbitMap with OrbitedBy = List.map (fun orbitMap -> orbitCouplesToOrbitMap orbitMap orbitCouples) newOrbitMap.OrbitedBy }

    let initialOrbitMap = { Name = "COM"; OrbitedBy = [] }

    orbitCouples
    |> orbitCouplesToOrbitMap initialOrbitMap

let getNumberOfDirectOrbits (orbitMap:OrbitMap) : int =
    orbitMap.OrbitedBy.Length

let rec getNumberOfDirectAndInderictOrbits (orbitMap:OrbitMap) : int =
    if List.isEmpty orbitMap.OrbitedBy then 0
    else
        (getNumberOfDirectOrbits orbitMap) + List.sumBy getNumberOfDirectAndInderictOrbits orbitMap.OrbitedBy

let rec calculateChecksum (orbitMap:OrbitMap) : int =
    let numberOfOrbits = getNumberOfDirectAndInderictOrbits orbitMap
    numberOfOrbits + (List.sumBy calculateChecksum orbitMap.OrbitedBy)

let fileToOrbitMap inputPath =
    File.ReadLines inputPath
    |> Seq.map orbitStringToCouple
    |> orbitCouplesToOrbitMap

let solveProblem1 inputPath =
    inputPath
    |> fileToOrbitMap
    |> calculateChecksum

let rec findShortestRouteTo object orbitMap : int =
    if (List.isEmpty orbitMap.OrbitedBy) then 999999999 // This is gross and really should be something like returning None (int option) but it works
    else if List.contains { Name = object; OrbitedBy = [] } orbitMap.OrbitedBy then 0
    else
        1 + List.min (List.map (fun orbitMap -> findShortestRouteTo object orbitMap) orbitMap.OrbitedBy)

let rec findShortestRouteBetween object1 object2 orbitMap : int =
    let shortestRouteToObject1 = findShortestRouteTo object1 orbitMap
    let shortestRouteToObject2 = findShortestRouteTo object2 orbitMap
    let shortestRouteBetweenObjects = shortestRouteToObject1 + shortestRouteToObject2
    List.min (shortestRouteBetweenObjects::(List.map (fun orbitMap -> (findShortestRouteBetween object1 object2 orbitMap)) orbitMap.OrbitedBy))

let solveProblem2 inputPath =
    inputPath
    |> fileToOrbitMap
    |> findShortestRouteBetween "YOU" "SAN"
