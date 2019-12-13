open System
open System.IO

let problemInputPath = Path.Combine(__SOURCE_DIRECTORY__, "input.txt")

let testInputPath testNumber = Path.Combine(__SOURCE_DIRECTORY__, sprintf "test%d.txt" testNumber)

type SpaceCoordinate = { X:int; Y:int; HasAsteroid:bool; }

let stringsToSpaceCoordinates (input:string seq) : SpaceCoordinate List =
    input
    |> Seq.mapi (fun y spaceMapLine ->
        Seq.mapi (fun x character ->
            if character = '#' then
                { X = x; Y = y; HasAsteroid = true }
            else
                { X = x; Y = y; HasAsteroid = false }) spaceMapLine)
    |> Seq.concat
    |> Seq.toList

let convertCoordinateToRelative coordinateOrigin coordinateToConvert =
    { coordinateToConvert with
        X = coordinateToConvert.X - coordinateOrigin.X;
        Y = coordinateToConvert.Y - coordinateOrigin.Y}

let calculateAngle spaceCoordinate =
    let x = spaceCoordinate.X
    let y = spaceCoordinate.Y
    atan2 (double y) (double x)

let getVisibleAsteroidsFromCoordinate coordinate spaceMap =
    spaceMap
    |> List.where (fun spaceCoordinate -> spaceCoordinate <> coordinate)
    |> List.map (fun spaceCoordinate -> convertCoordinateToRelative coordinate spaceCoordinate)
    |> List.distinctBy calculateAngle
    |> List.length

let getSpaceMapFromInput inputPath =
    File.ReadLines inputPath |> stringsToSpaceCoordinates

let solveProblem1 inputPath =
    let spaceMap = getSpaceMapFromInput inputPath |> List.where (fun spaceCoordinate -> spaceCoordinate.HasAsteroid)
    spaceMap
    |> List.map (fun coordinate -> getVisibleAsteroidsFromCoordinate coordinate spaceMap)
    |> List.max
