open System
open System.IO

let problemInputPath = Path.Combine(__SOURCE_DIRECTORY__, "input.txt")
let testInputPath testNumber = Path.Combine(__SOURCE_DIRECTORY__, sprintf "test%d.txt" testNumber)

let stringToIntcode (textInput:string) =
    textInput.Split([|','|])
    |> Array.mapi (fun index value -> (index, int64 value))
    |> Map.ofArray

let readFileToIntcode path =
    Path.Combine(__SOURCE_DIRECTORY__, path)
    |> File.ReadLines
    |> Seq.head
    |> stringToIntcode

let intcodeWithValueAtLocation storageLocation operationResult (intcode:Map<int, int64>) =
    Map.add storageLocation operationResult intcode

let getValueOfDigit number digitPlace =
    let powerOfTen =  int64 (Math.Pow(10.0, (float digitPlace)))
    let nextPowerOfTen = powerOfTen * 10L
    ((number % nextPowerOfTen) - (number % powerOfTen)) / powerOfTen

let getValueFromIntcode index intcode =
    if Map.containsKey index intcode then
        (intcode.[index], intcode)
    else (0L, Map.add index 0L intcode)

let getValue (intcode:(Map<int, int64>)) (parameter:int64) (parameterMode:int64) (relativeBase:int) =
    match parameterMode with
    | 0L -> getValueFromIntcode (int parameter) intcode
    | 1L -> (parameter, intcode)
    | 2L -> getValueFromIntcode (relativeBase + (int parameter)) intcode
    | _ -> failwith "Unsupported parameter mode"

let lessThan x y =
    if x < y then 1L
    else 0L

let equal x y =
    if x = y then 1L
    else 0L

type IntcodeComputer = {
    InstructionPointer : int;
    Input : int64 list;
    Output : int64 list;
    Intcode : Map<int, int64>;
    ProgramCompleted : bool;
    RelativeBase: int;
}

let rec runIntcodeComputer (intcodeComputer:IntcodeComputer) =
    // i is the operation we're executing
    // i + 1 is position of first input
    // i + 2 is position of second input
    // i + 3 is where we want to store results
    // i + 4 is next operation
    let applyAndStoreOperation operation (intcodeComputer:IntcodeComputer) =
        let opcode = intcodeComputer.Intcode.[intcodeComputer.InstructionPointer]

        let firstParameterMode = getValueOfDigit opcode 2
        let secondParameterMode = getValueOfDigit opcode 3
        let thirdParameterMode = getValueOfDigit opcode 4

        let (firstParameterValue, _) = getValue intcodeComputer.Intcode intcodeComputer.Intcode.[intcodeComputer.InstructionPointer + 1] firstParameterMode intcodeComputer.RelativeBase
        let (secondParameterValue, _ ) = getValue intcodeComputer.Intcode intcodeComputer.Intcode.[intcodeComputer.InstructionPointer + 2] secondParameterMode intcodeComputer.RelativeBase

        let operationResult = operation firstParameterValue secondParameterValue
        let storageLocation =
            if thirdParameterMode = 0L then intcodeComputer.Intcode.[intcodeComputer.InstructionPointer + 3]
            else (intcodeComputer.Intcode.[intcodeComputer.InstructionPointer + 3] + (int64 intcodeComputer.RelativeBase))

        runIntcodeComputer { intcodeComputer with
                                InstructionPointer = intcodeComputer.InstructionPointer + 4;
                                Intcode = (intcodeWithValueAtLocation (int storageLocation) operationResult intcodeComputer.Intcode) }

    // i is the operation we're executing
    // i + 1 is the position we want to store our input
    let storeInputOperation intcodeComputer =
        let inputValue = List.tryHead intcodeComputer.Input

        let opcode = intcodeComputer.Intcode.[intcodeComputer.InstructionPointer]

        let firstParameterMode = getValueOfDigit opcode 2
        let storageLocation =
            if firstParameterMode = 0L then intcodeComputer.Intcode.[intcodeComputer.InstructionPointer + 1]
            else (intcodeComputer.Intcode.[intcodeComputer.InstructionPointer + 1] + (int64 intcodeComputer.RelativeBase))

        match inputValue with
        | None -> { intcodeComputer with ProgramCompleted = false; }
        | Some inputValue -> runIntcodeComputer {
            intcodeComputer with
                InstructionPointer = intcodeComputer.InstructionPointer + 2;
                Input = (List.tail intcodeComputer.Input)
                Intcode = (intcodeWithValueAtLocation (int storageLocation) inputValue intcodeComputer.Intcode) }

    // i is the operation we're executing
    // i + 1 is the position we want to output
    let outputOperation intcodeComputer =
        let opcode = intcodeComputer.Intcode.[intcodeComputer.InstructionPointer]
        let firstParameterMode = getValueOfDigit opcode 2
        let (outputValue, _) = getValue intcodeComputer.Intcode intcodeComputer.Intcode.[intcodeComputer.InstructionPointer + 1] firstParameterMode intcodeComputer.RelativeBase
        runIntcodeComputer { intcodeComputer with
                                InstructionPointer = intcodeComputer.InstructionPointer + 2;
                                Output = outputValue::intcodeComputer.Output; }

    let jumpOnComparisonToZero operation (intcodeComputer:IntcodeComputer) =
        let opcode = intcodeComputer.Intcode.[intcodeComputer.InstructionPointer]

        let firstParameterMode = getValueOfDigit opcode 2
        let secondParameterMode = getValueOfDigit opcode 3

        let (firstParameterValue, _) = getValue intcodeComputer.Intcode intcodeComputer.Intcode.[intcodeComputer.InstructionPointer + 1] firstParameterMode intcodeComputer.RelativeBase
        let (secondParameterValue, _) = getValue intcodeComputer.Intcode intcodeComputer.Intcode.[intcodeComputer.InstructionPointer + 2] secondParameterMode intcodeComputer.RelativeBase

        if operation firstParameterValue 0L then
            runIntcodeComputer { intcodeComputer with
                                    InstructionPointer = int secondParameterValue; }
        else
            runIntcodeComputer { intcodeComputer with
                                    InstructionPointer = (intcodeComputer.InstructionPointer + 3); }

    let relativeBaseOffsetOperation intcodeComputer =
        let opcode = intcodeComputer.Intcode.[intcodeComputer.InstructionPointer]
        let firstParameterMode = getValueOfDigit opcode 2

        let (firstParameterValue, _) = getValue intcodeComputer.Intcode intcodeComputer.Intcode.[intcodeComputer.InstructionPointer + 1] firstParameterMode intcodeComputer.RelativeBase

        runIntcodeComputer { intcodeComputer with
                                InstructionPointer = (intcodeComputer.InstructionPointer + 2);
                                RelativeBase = (intcodeComputer.RelativeBase + int firstParameterValue); }

    let opcode = intcodeComputer.Intcode.[intcodeComputer.InstructionPointer] % 100L
    match opcode with
    | 99L -> { intcodeComputer with
                Output = intcodeComputer.Output |> List.rev;
                ProgramCompleted = true; }
    | 1L -> applyAndStoreOperation (+) intcodeComputer
    | 2L -> applyAndStoreOperation (*) intcodeComputer
    | 3L -> storeInputOperation intcodeComputer
    | 4L -> outputOperation intcodeComputer
    | 5L -> jumpOnComparisonToZero (<>) intcodeComputer
    | 6L -> jumpOnComparisonToZero (=) intcodeComputer
    | 7L -> applyAndStoreOperation lessThan intcodeComputer
    | 8L -> applyAndStoreOperation equal intcodeComputer
    | 9L -> relativeBaseOffsetOperation intcodeComputer
    | _ -> failwith "Invalid op code encountered"

let runIntcode (input:int64 list) (intcode:Map<int, int64>) : IntcodeComputer =
    runIntcodeComputer { InstructionPointer = 0; Input = input; Output = []; Intcode = intcode; ProgramCompleted = false; RelativeBase = 0; }

let numberOfBlocks gameState =
    gameState
    |> (fun intcodeComputer-> Seq.chunkBySize 3 intcodeComputer.Output)
    |> Seq.where (fun tileSpecs -> tileSpecs.[2] = 2L)
    |> Seq.length

let solveProblem1 inputPath =
    inputPath
    |> readFileToIntcode
    |> runIntcode []
    |> numberOfBlocks

let rec playGame intcodeComputer =
    let blocksRemaining = numberOfBlocks intcodeComputer
    printfn "Blocks remaining %d" blocksRemaining
    if not (List.isEmpty intcodeComputer.Output) && blocksRemaining = 0 then
        intcodeComputer.Output
    else
        let tiles = Seq.chunkBySize 3 intcodeComputer.Output
        let (paddleX, _) = (Seq.tryFind (fun (tileSpecs:int64 array) -> tileSpecs.[2] = 3L) tiles)
                            |> (fun tileSpecs ->
                                match tileSpecs with
                                | None -> (0L, 0L)
                                | Some tileSpecs -> (tileSpecs.[0], tileSpecs.[1]))
        let (ballX, _) = (Seq.tryFind (fun (tileSpecs:int64 array) -> tileSpecs.[2] = 4L) tiles)
                            |> (fun tileSpecs ->
                                match tileSpecs with
                                | None -> (paddleX, 0L)
                                | Some tileSpecs -> (tileSpecs.[0], tileSpecs.[1]))

        let input = if ballX < paddleX then -1L
                    elif ballX = paddleX then 0L
                    else 1L

        playGame { intcodeComputer with Input = [input]; Output = []; }

let solveProblem2 inputPath =
    inputPath
    |> readFileToIntcode
    |> Map.add 0 2L
    |> runIntcode [0L]
    |> playGame
