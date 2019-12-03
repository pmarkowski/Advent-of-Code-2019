open System.IO

let inputFile = "input.txt"

let stringToInstructionList (textInput:string) =
    textInput.Split([|','|])
    |> Array.map int
    |> Array.toList

let readFileToInstructionText path =
    Path.Combine(__SOURCE_DIRECTORY__, path)
    |> File.ReadLines
    |> Seq.head
    |> stringToInstructionList

let intCodeWithValueAtLocation storageLocation operationResult intcode =
    intcode
    |> List.mapi (fun i value ->
        if i = storageLocation then operationResult
        else value)

// i is the operation we're executing
// i + 1 is position of first input
// i + 2 is position of second input
// i + 3 is where we want to store results
// i + 4 is next operation
let rec runIntCode i (intcode:int list) =
    let applyAndStoreOperation i operation (intcode:int list)  =
        let operationResult = operation intcode.[intcode.[i + 1]] intcode.[intcode.[i + 2]]
        let storageLocation = intcode.[i + 3]
        runIntCode (i + 4) (intCodeWithValueAtLocation storageLocation operationResult intcode) 
    let opCode = intcode.[i]
    match opCode with
    | 99 -> intcode
    | 1 -> applyAndStoreOperation i (+) intcode
    | 2 -> applyAndStoreOperation i (*) intcode
    | _ -> failwith "Invalid op code encountered"

let solvePart1 inputFile =
    inputFile
    |> readFileToInstructionText
    |> intCodeWithValueAtLocation 1 12
    |> intCodeWithValueAtLocation 2 2
    |> runIntCode 0
    |> List.head

// Part 2
