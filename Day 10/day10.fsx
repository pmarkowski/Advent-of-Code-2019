open System
open System.IO

let problemInputPath = Path.Combine(__SOURCE_DIRECTORY__, "input.txt")

let testInputPath testNumber = Path.Combine(__SOURCE_DIRECTORY__, sprintf "test%d.txt" testNumber)

type SpaceCoordinate = { X:int; Y:int; HasAsteroid:bool; OriginalX:int; OriginalY:int; }

let getSpaceDistance spaceCoordinate =
    abs spaceCoordinate.X + abs spaceCoordinate.Y

let stringsToSpaceCoordinates (input:string seq) : SpaceCoordinate List =
    input
    |> Seq.mapi (fun y spaceMapLine ->
        Seq.mapi (fun x character ->
            if character = '#' then
                { X = x; Y = y; HasAsteroid = true; OriginalX = x; OriginalY = y; }
            else
                { X = x; Y = y; HasAsteroid = false; OriginalX = x; OriginalY = y; }) spaceMapLine)
    |> Seq.concat
    |> Seq.toList

let convertCoordinateToRelative coordinateOrigin coordinateToConvert =
    { coordinateToConvert with
        X = coordinateToConvert.X - coordinateOrigin.X;
        Y = coordinateToConvert.Y - coordinateOrigin.Y}

let calculateAngleFromYAxis spaceCoordinate =
    let x = spaceCoordinate.X
    let y = spaceCoordinate.Y
    let angleFromYAxis = atan2 (double x) -(double y)
    match angleFromYAxis with
    | angle when angle < 0.0 -> (Math.PI * 2.0) + angle
    | angle -> angle

let addValueToMapList key value map =
    if Map.containsKey key map then Map.add key (List.sortBy getSpaceDistance (value::(map.[key]))) map
    else Map.add key [value] map

let getVisibleAsteroidsFromCoordinate coordinate spaceMap =
    spaceMap
    |> List.where (fun spaceCoordinate -> spaceCoordinate <> coordinate)
    |> List.map (fun spaceCoordinate -> convertCoordinateToRelative coordinate spaceCoordinate)
    |> List.fold (fun asteroidAngleMap spaceCoordinate ->
        addValueToMapList (calculateAngleFromYAxis spaceCoordinate) spaceCoordinate asteroidAngleMap) Map.empty

let getSpaceMapFromInput inputPath =
    File.ReadLines inputPath |> stringsToSpaceCoordinates

let solveProblem1 inputPath =
    let spaceMap = getSpaceMapFromInput inputPath |> List.where (fun spaceCoordinate -> spaceCoordinate.HasAsteroid)
    spaceMap
    |> List.map ((fun coordinate -> getVisibleAsteroidsFromCoordinate coordinate spaceMap) >> Map.count)
    |> List.max

let removeAsteroidFromList
    (asteroid:SpaceCoordinate)
    (asteroidList:(float * SpaceCoordinate list) list)
    : (float * SpaceCoordinate list) list =
    asteroidList
    |> List.map (fun asteroidPair -> (fst asteroidPair, List.except [asteroid] (snd asteroidPair)))
    |> List.where (fun asteroidPair -> not (List.isEmpty (snd asteroidPair)))

let getNthAsteroid asteroidToHit asteroidMap : SpaceCoordinate =
    let rec getNthAsteroid (angleIndex:int) remainingAsteroids (asteroidList:(float * SpaceCoordinate list) list) =
        let asteroidLaserIsPointingAt = (List.head (snd asteroidList.[angleIndex]))
        if remainingAsteroids = 1 then asteroidLaserIsPointingAt
        else
            let newAsteroidList = removeAsteroidFromList asteroidLaserIsPointingAt asteroidList
            if newAsteroidList.Length < asteroidList.Length then
                getNthAsteroid angleIndex (remainingAsteroids - 1) newAsteroidList
            else
                getNthAsteroid ((angleIndex + 1) % asteroidList.Length) (remainingAsteroids - 1) newAsteroidList

    getNthAsteroid 0 asteroidToHit (Map.toList asteroidMap)

let convertCoordinateToAbsolute coordinateOrigin coordinateToConvert =
    {coordinateToConvert with 
        X = coordinateToConvert.X + coordinateOrigin.X;
        Y = coordinateToConvert.Y + coordinateOrigin.Y}

let solveProblem2 inputPath asteroidToHit =
    let spaceMap = getSpaceMapFromInput inputPath |> List.where (fun spaceCoordinate -> spaceCoordinate.HasAsteroid)
    let bestAsteroid = spaceMap |> List.maxBy (fun coordinate -> (getVisibleAsteroidsFromCoordinate coordinate spaceMap) |> Map.count)
    spaceMap
    |> getVisibleAsteroidsFromCoordinate bestAsteroid
    |> getNthAsteroid asteroidToHit
    |> (fun coord -> coord.OriginalX * 100 + coord.OriginalY)
