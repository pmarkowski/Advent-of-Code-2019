open System.IO
open System

let problemInputPath = Path.Combine(__SOURCE_DIRECTORY__, "input.txt")
let testInputPath testNumber = Path.Combine(__SOURCE_DIRECTORY__, sprintf "test%d.txt" testNumber)

type Moon = {
    Position: int * int * int;
    Velocity: int * int * int;
}

let stringToMoon (string:string) =
    string.Split([|"<";",";">";" ";"x=";"y=";"z="|], StringSplitOptions.RemoveEmptyEntries)
    |> (fun stringArray ->
        {
            Position = (int stringArray.[0], int stringArray.[1], int stringArray.[2]);
            Velocity = (0, 0, 0)
        })

let parseStringsToMoons strings =
    strings
    |> Seq.map stringToMoon
    |> Seq.toList
    
let addVectors (x1, y1, z1) (x2, y2, z2) =
    (x1 + x2, y1 + y2, z1 + z2)


let getGravityDelta (x1, y1, z1) (x2, y2, z2) =
    let getGravityDelta a b =
        if a < b then 1
        elif a = b then 0
        else -1
    (getGravityDelta x1 x2, getGravityDelta y1 y2, getGravityDelta z1 z2)

let applyGravityFromOneMoon moonToUpdate secondMoon =
    let gravityDeltaVector = getGravityDelta moonToUpdate.Position secondMoon.Position
    { moonToUpdate with Velocity = addVectors moonToUpdate.Velocity gravityDeltaVector }

let applyGravity moonToUpdate moons =
    moons
    |> List.where (fun moon -> moon <> moonToUpdate)
    |> List.fold applyGravityFromOneMoon moonToUpdate

let updateMoonVelocity moons =
    moons
    |> List.map (fun moon -> applyGravity moon moons)


let updateMoonPosition moon =
    { moon with Position = addVectors moon.Position moon.Velocity }

let updateMoonPositions moons =
    moons
    |> List.map updateMoonPosition

let rec simulateSteps stepsToSimulate moons =
    match stepsToSimulate with
    | 0 -> moons
    | stepsToSimulate ->
        let moonsWithNewVelocity = updateMoonVelocity moons
        let moonsWithNewPositions = updateMoonPositions moonsWithNewVelocity
        simulateSteps (stepsToSimulate - 1) moonsWithNewPositions

let getTripleSum (x,y,z) =
    abs x + abs y + abs z

let getMoonEnergy moon =
    getTripleSum moon.Position * getTripleSum moon.Velocity

let addMoonEnergy totalEnergy moon =
    totalEnergy + (getMoonEnergy moon)

let calculateTotalEnergy moons : int =
    moons
    |> List.fold addMoonEnergy 0

let solveProblem1 inputPath =
    inputPath
    |> File.ReadAllLines
    |> parseStringsToMoons
    |> simulateSteps 1000
    |> calculateTotalEnergy
