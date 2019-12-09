open System
open System.IO

let inputPath = Path.Combine(__SOURCE_DIRECTORY__, "input.txt")

let solveProblem1 inputPath =
    let layerWithFewestZeroes = File.ReadLines inputPath |> Seq.head |> Seq.chunkBySize (25 * 6) |> Seq.minBy (fun layer -> (Array.where (fun character -> character = '0') layer).Length)
    (Seq.length (Seq.where (fun character -> character = '1') layerWithFewestZeroes)) * (Seq.length (Seq.where (fun character -> character = '2') layerWithFewestZeroes))

let getNextColour topColour bottomColour =
    match topColour with
    | '2' -> bottomColour
    | _ -> topColour

let getNextLayer topLayer nextLayer =
    Array.map2 getNextColour topLayer nextLayer

let rec printLayer width layer : unit =
    let lineToPrint = String (Array.map (fun character ->
        if character = '1' then '*'
        else ' ')
        (Array.take width layer))
    printfn "%s" lineToPrint
    printLayer width (Array.skip width layer)

let solveProblem2 inputPath =
    let transparentInitialLayer = (Array.init (25 * 6) (fun i -> '2'))
    File.ReadLines inputPath
    |> Seq.head
    |> Seq.chunkBySize (25 * 6)
    |> Seq.fold (fun finalImage layer -> getNextLayer finalImage layer) transparentInitialLayer
    |> printLayer 25

