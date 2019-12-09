open System
open System.IO

let inputPath = Path.Combine(__SOURCE_DIRECTORY__, "input.txt")

let solveProblem1 inputPath =
    let layerWithFewestZeroes = File.ReadLines inputPath |> Seq.head |> Seq.chunkBySize (25 * 6) |> Seq.minBy (fun layer -> (Array.where (fun character -> character = '0') layer).Length)
    (Seq.length (Seq.where (fun character -> character = '1') layerWithFewestZeroes)) * (Seq.length (Seq.where (fun character -> character = '2') layerWithFewestZeroes))