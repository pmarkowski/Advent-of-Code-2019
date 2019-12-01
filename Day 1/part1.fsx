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
    