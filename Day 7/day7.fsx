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

let runIntcode (input:int list) (intcode:int list) : int list =
    let rec runIntcode i (input:int list) (output:int list) (intcode:int list) =
        // i is the operation we're executing
        // i + 1 is position of first input
        // i + 2 is position of second input
        // i + 3 is where we want to store results
        // i + 4 is next operation
        let applyAndStoreOperation i input output operation (intcode:int list)  =
            let opcode = intcode.[i]

            let firstParameterMode = getValueOfDigit opcode 2
            let secondParameterMode = getValueOfDigit opcode 3

            let firstParameterValue = getValue intcode intcode.[i + 1] firstParameterMode
            let secondParameterValue = getValue intcode intcode.[i + 2] secondParameterMode

            let operationResult = operation firstParameterValue secondParameterValue
            let storageLocation = intcode.[i + 3]

            runIntcode (i + 4) input output (intcodeWithValueAtLocation storageLocation operationResult intcode)

        // i is the operation we're executing
        // i + 1 is the position we want to store our input
        let storeInputOperation i input output (intcode:int list) =
            let inputValue = List.head input
            runIntcode (i + 2) (List.tail input) output (intcodeWithValueAtLocation intcode.[i+1] inputValue intcode)

        // i is the operation we're executing
        // i + 1 is the position we want to output
        let outputOperation i input (output:int list) (intcode:int list) =
            let opcode = intcode.[i]
            let firstParameterMode = getValueOfDigit opcode 2
            let outputValue = getValue intcode intcode.[i+1] firstParameterMode
            runIntcode (i + 2) input (outputValue::output) intcode

        let jumpOnComparisonToZero i input output operation (intcode:int list) =
            let opcode = intcode.[i]

            let firstParameterMode = getValueOfDigit opcode 2
            let secondParameterMode = getValueOfDigit opcode 3

            let firstParameterValue = getValue intcode intcode.[i + 1] firstParameterMode
            let secondParameterValue = getValue intcode intcode.[i + 2] secondParameterMode

            if operation firstParameterValue 0 then
                runIntcode secondParameterValue input output intcode
            else
                runIntcode (i + 3) input output intcode

        let opcode = intcode.[i] % 100
        match opcode with
        | 99 -> output |> List.rev
        | 1 -> applyAndStoreOperation i input output (+) intcode
        | 2 -> applyAndStoreOperation i input output (*) intcode
        | 3 -> storeInputOperation i input output intcode
        | 4 -> outputOperation i input output intcode
        | 5 -> jumpOnComparisonToZero i input output (<>) intcode
        | 6 -> jumpOnComparisonToZero i input output (=) intcode
        | 7 -> applyAndStoreOperation i input output lessThan intcode
        | 8 -> applyAndStoreOperation i input output equal intcode
        | _ -> failwith "Invalid op code encountered"

    runIntcode 0 input [] intcode

let test1PhaseSettings = [4;3;2;1;0]
let test1Intcode = stringToIntcode "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"
let test1Max = 43210

let test2PhaseSettings = [0;1;2;3;4]
let test2Intcode = stringToIntcode "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0"
let test2Max = 54321

let test3PhaseSettings = [1;0;4;3;2]
let test3Intcode = stringToIntcode "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"
let test3Max = 65210

let rec runAmplifierControllers (input:int) (phaseArray:int list) (intcode:int list) =

    let runAmplifierController (phase:int, input:int) (intcode:int list) =
        runIntcode [phase; input] intcode
        |> List.head

    if List.isEmpty phaseArray then input
    else
        let firstControllerOutput = runAmplifierController (List.head phaseArray, input) intcode
        runAmplifierControllers firstControllerOutput (List.tail phaseArray) intcode

let rec getAllPermutations array =

    let getEachValueWithRestOfValues array =
        array
        |> List.map (fun x -> (x, (List.where (fun y -> x <> y) array)))

    if List.isEmpty array then [[]]
    else
        array
        |> getEachValueWithRestOfValues
        |> List.collect (fun (first, rest) ->
            getAllPermutations rest
            |> List.map (fun rest -> first::rest))

let solveProblem1 inputPath =
    let inputIntcode = readFileToIntcode inputPath
    let allPhasePermutations = getAllPermutations [0..4]
    List.max (List.map (fun phasePermutation -> runAmplifierControllers 0 phasePermutation inputIntcode) allPhasePermutations)
