open System
open System.IO

let inputFile = "input.txt"

let stringToIntcode (textInput:string) =
    textInput.Split([|','|])
    |> Array.map int
    |> Array.toList

let readFileToIntcode path =
    Path.Combine(__SOURCE_DIRECTORY__, path)
    |> File.ReadLines
    |> Seq.head
    |> stringToIntcode

let intcodeWithValueAtLocation storageLocation operationResult intcode =
    intcode
    |> List.mapi (fun i value ->
        if i = storageLocation then operationResult
        else value)

let getValueOfDigit number digitPlace =
    let powerOfTen =  int (Math.Pow(10.0, (float digitPlace)))
    let nextPowerOfTen = powerOfTen * 10
    ((number % nextPowerOfTen) - (number % powerOfTen)) / powerOfTen

let getValue (intcode:int list) (parameter:int) (parameterMode:int) =
    match parameterMode with
    | 0 -> intcode.[parameter]
    | 1 -> parameter
    | _ -> failwith "Unsupported parameter mode"

let lessThan x y =
    if x < y then 1
    else 0

let equal x y =
    if x = y then 1
    else 0

let rec runIntcode i (intcode:int list) =
    // i is the operation we're executing
    // i + 1 is position of first input
    // i + 2 is position of second input
    // i + 3 is where we want to store results
    // i + 4 is next operation
    let applyAndStoreOperation i operation (intcode:int list)  =
        let opcode = intcode.[i]

        let firstParameterMode = getValueOfDigit opcode 2
        let secondParameterMode = getValueOfDigit opcode 3

        let firstParameterValue = getValue intcode intcode.[i + 1] firstParameterMode
        let secondParameterValue = getValue intcode intcode.[i + 2] secondParameterMode

        let operationResult = operation firstParameterValue secondParameterValue
        let storageLocation = intcode.[i + 3]

        runIntcode (i + 4) (intcodeWithValueAtLocation storageLocation operationResult intcode)

    // i is the operation we're executing
    // i + 1 is the position we want to store our input
    let storeInputOperation i (intcode:int list) =
        printfn "Waiting for input."
        let input = int (Console.ReadLine())
        runIntcode (i + 2) (intcodeWithValueAtLocation intcode.[i+1] input intcode)

    // i is the operation we're executing
    // i + 1 is the position we want to output
    let outputOperation i (intcode:int list) =
        let opcode = intcode.[i]
        let firstParameterMode = getValueOfDigit opcode 2
        let output = getValue intcode intcode.[i+1] firstParameterMode
        printfn "%d" output
        runIntcode (i + 2) intcode

    let jumpOnComparisonToZero i operation (intcode:int list) =
        let opcode = intcode.[i]

        let firstParameterMode = getValueOfDigit opcode 2
        let secondParameterMode = getValueOfDigit opcode 3

        let firstParameterValue = getValue intcode intcode.[i + 1] firstParameterMode
        let secondParameterValue = getValue intcode intcode.[i + 2] secondParameterMode

        if operation firstParameterValue 0 then
            runIntcode secondParameterValue intcode
        else
            runIntcode (i + 3) intcode

    let opcode = intcode.[i] % 100
    match opcode with
    | 99 -> intcode
    | 1 -> applyAndStoreOperation i (+) intcode
    | 2 -> applyAndStoreOperation i (*) intcode
    | 3 -> storeInputOperation i intcode
    | 4 -> outputOperation i intcode
    | 5 -> jumpOnComparisonToZero i (<>) intcode
    | 6 -> jumpOnComparisonToZero i (=) intcode
    | 7 -> applyAndStoreOperation i lessThan intcode
    | 8 -> applyAndStoreOperation i equal intcode
    | _ -> failwith "Invalid op code encountered"

let solvePart1 inputFile =
    inputFile
    |> readFileToIntcode
    |> runIntcode 0
