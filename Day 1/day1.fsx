open System.IO

let inputFile = "input.txt"

let readFileToIntegerList (path:string) =
    Path.Combine(__SOURCE_DIRECTORY__, path)
    |> File.ReadLines
    |> Seq.map int
    |> Seq.toList

let calculateFuelNeededByMass (mass:int) =
    int (floor ((double mass) / 3.0) - 2.0)

let getTotalFuelNeededToLaunchMass (path:string) =
    readFileToIntegerList path
    |> List.map calculateFuelNeededByMass
    |> List.sum

// Part 2

let rec calculateFuelNeededByMassIncludingFuel (mass:int) =
    if mass = 0 then 0
    else 
        let fuelRequired = max 0 (calculateFuelNeededByMass mass)
        fuelRequired + (calculateFuelNeededByMassIncludingFuel fuelRequired)

let getTotalFuelNeededToLaunchMassIncludingFuel (path:string) =
    readFileToIntegerList path
    |> List.map calculateFuelNeededByMassIncludingFuel
    |> List.sum
