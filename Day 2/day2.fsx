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
let intCodeWithInputs (noun, verb) intcode =
    intcode
    |> intCodeWithValueAtLocation 1 noun
    |> intCodeWithValueAtLocation 2 verb

let runIntCodeWithInputs (noun, verb) intcode =
    intCodeWithInputs (noun, verb) intcode
    |> runIntCode 0
    |> List.head

let findInputsForCodeWithResult result intcode =
    let rec findInputsForCodeWithResult result (noun, verb) intcode =
        let intCodeOutput = runIntCodeWithInputs (noun, verb) intcode
        if intCodeOutput = result then (noun, verb)
        else if verb < 99 then findInputsForCodeWithResult result (noun, verb + 1) intcode
        else findInputsForCodeWithResult result (noun + 1, 0) intcode
    findInputsForCodeWithResult result (0, 0) intcode

let solvePart2 inputFile =
    inputFile
    |> readFileToInstructionText
    |> findInputsForCodeWithResult 19690720
    |> fun (noun, verb) -> 100 * noun + verb

let part1Soln = solvePart2 inputFile
