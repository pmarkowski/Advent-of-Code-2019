open System.IO

let inputPath = Path.Combine(__SOURCE_DIRECTORY__, "input.txt")

let calculateManhattanDistance (x:int, y:int) =
    abs x + abs y

let inline (+) (x:int, y:int) (x2:int, y2:int) =
    (x + x2, y + y2)

type Direction = { direction: string; distance: int }

let readStringToDirectionList (wirePath:string) =
    wirePath.Split([|','|])
    |> Array.map (fun directionString -> {
        direction = directionString.[0..0];
        distance = (int directionString.[1..])})
    |> Array.toList

let getIteratorForDirection currentDirection =
    match currentDirection.direction with
    | "U" -> (0, 1)
    | "R" -> (1, 0)
    | "D" -> (0, -1)
    | "L" -> (-1, 0)
    | _ -> failwith "Unsupported direction"

let rec moveCoordinateTrailByDirection (currentDirection:Direction) coordinateTrail =
    let currentPosition = List.head coordinateTrail
    let iterator = getIteratorForDirection currentDirection
    if currentDirection.distance = 0 then coordinateTrail
    else
    (currentPosition + iterator)::coordinateTrail
    |> moveCoordinateTrailByDirection {currentDirection with distance = currentDirection.distance - 1}

let directionListToCoordinateList directionList =
    let rec directionListToCoordinateList coordinateTrail directionList =
        let currentDirection = List.tryHead directionList
        match currentDirection with
        | None -> List.rev coordinateTrail
        | Some currentDirection ->
            directionListToCoordinateList
                (moveCoordinateTrailByDirection currentDirection coordinateTrail)
                (List.tail directionList)
    directionList
    |> directionListToCoordinateList [(0,0)]

let getAllIntersectingCoordinates coordinateList1 coordinateList2 =
    coordinateList1
    |> Set.filter (fun coordinate ->
        coordinate <> (0,0) &&
        Set.contains coordinate coordinateList2)

let getLowestManhattanDistance coordinateList =
    coordinateList
    |> List.minBy calculateManhattanDistance
    |> calculateManhattanDistance

let getInputPair inputPath =
    File.ReadLines inputPath
        |> Seq.map readStringToDirectionList
        |> Seq.map directionListToCoordinateList
        |> Seq.map Set.ofList

let solveProblem1 inputPath =
    let inputPair = getInputPair inputPath
    getAllIntersectingCoordinates (Seq.head inputPair) (Seq.head (Seq.tail inputPair))
    |> Set.toList
    |> getLowestManhattanDistance
