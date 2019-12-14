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

let getValue (intcode:int list) (parameter:int) (parameterMode:int) (relativeBase:int) =
    match parameterMode with
    | 0 -> intcode.[parameter]
    | 1 -> parameter
    | 2 -> intcode.[relativeBase + parameter]
    | _ -> failwith "Unsupported parameter mode"

let lessThan x y =
    if x < y then 1
    else 0

let equal x y =
    if x = y then 1
    else 0

type IntcodeComputer = {
    InstructionPointer : int;
    Input : int list;
    Output : int list;
    Intcode : int list;
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

        let firstParameterValue = getValue intcodeComputer.Intcode intcodeComputer.Intcode.[intcodeComputer.InstructionPointer + 1] firstParameterMode intcodeComputer.RelativeBase
        let secondParameterValue = getValue intcodeComputer.Intcode intcodeComputer.Intcode.[intcodeComputer.InstructionPointer + 2] secondParameterMode intcodeComputer.RelativeBase

        let operationResult = operation firstParameterValue secondParameterValue
        let storageLocation = intcodeComputer.Intcode.[intcodeComputer.InstructionPointer + 3]

        runIntcodeComputer { intcodeComputer with
                                InstructionPointer = intcodeComputer.InstructionPointer + 4;
                                Intcode = (intcodeWithValueAtLocation storageLocation operationResult intcodeComputer.Intcode) }

    // i is the operation we're executing
    // i + 1 is the position we want to store our input
    let storeInputOperation intcodeComputer =
        let inputValue = List.tryHead intcodeComputer.Input
        match inputValue with
        | None -> { intcodeComputer with ProgramCompleted = false; }
        | Some inputValue -> runIntcodeComputer {
            intcodeComputer with
                InstructionPointer = intcodeComputer.InstructionPointer + 2;
                Input = (List.tail intcodeComputer.Input)
                Intcode = (intcodeWithValueAtLocation intcodeComputer.Intcode.[intcodeComputer.InstructionPointer + 1] inputValue intcodeComputer.Intcode) }

    // i is the operation we're executing
    // i + 1 is the position we want to output
    let outputOperation intcodeComputer =
        let opcode = intcodeComputer.Intcode.[intcodeComputer.InstructionPointer]
        let firstParameterMode = getValueOfDigit opcode 2
        let outputValue = getValue intcodeComputer.Intcode intcodeComputer.Intcode.[intcodeComputer.InstructionPointer + 1] firstParameterMode intcodeComputer.RelativeBase
        runIntcodeComputer { intcodeComputer with
                                InstructionPointer = intcodeComputer.InstructionPointer + 2;
                                Output = outputValue::intcodeComputer.Output; }

    let jumpOnComparisonToZero operation (intcodeComputer:IntcodeComputer) =
        let opcode = intcodeComputer.Intcode.[intcodeComputer.InstructionPointer]

        let firstParameterMode = getValueOfDigit opcode 2
        let secondParameterMode = getValueOfDigit opcode 3

        let firstParameterValue = getValue intcodeComputer.Intcode intcodeComputer.Intcode.[intcodeComputer.InstructionPointer + 1] firstParameterMode intcodeComputer.RelativeBase
        let secondParameterValue = getValue intcodeComputer.Intcode intcodeComputer.Intcode.[intcodeComputer.InstructionPointer + 2] secondParameterMode intcodeComputer.RelativeBase

        if operation firstParameterValue 0 then
            runIntcodeComputer { intcodeComputer with
                                    InstructionPointer = secondParameterValue; }
        else
            runIntcodeComputer { intcodeComputer with
                                    InstructionPointer = (intcodeComputer.InstructionPointer + 3); }

    let relativeBaseOffsetOperation intcodeComputer =
        let opcode = intcodeComputer.Intcode.[intcodeComputer.InstructionPointer]
        let firstParameterMode = getValueOfDigit opcode 2

        let firstParameterValue = getValue intcodeComputer.Intcode intcodeComputer.Intcode.[intcodeComputer.InstructionPointer + 1] firstParameterMode intcodeComputer.RelativeBase

        runIntcodeComputer { intcodeComputer with
                                InstructionPointer = (intcodeComputer.InstructionPointer + 2);
                                RelativeBase = (intcodeComputer.RelativeBase + firstParameterValue); }

    let opcode = intcodeComputer.Intcode.[intcodeComputer.InstructionPointer] % 100
    match opcode with
    | 99 -> { intcodeComputer with
                Output = intcodeComputer.Output |> List.rev;
                ProgramCompleted = true; }
    | 1 -> applyAndStoreOperation (+) intcodeComputer
    | 2 -> applyAndStoreOperation (*) intcodeComputer
    | 3 -> storeInputOperation intcodeComputer
    | 4 -> outputOperation intcodeComputer
    | 5 -> jumpOnComparisonToZero (<>) intcodeComputer
    | 6 -> jumpOnComparisonToZero (=) intcodeComputer
    | 7 -> applyAndStoreOperation lessThan intcodeComputer
    | 8 -> applyAndStoreOperation equal intcodeComputer
    | 9 -> relativeBaseOffsetOperation intcodeComputer
    | _ -> failwith "Invalid op code encountered"

let runIntcode (input:int list) (intcode:int list) : IntcodeComputer =
    runIntcodeComputer { InstructionPointer = 0; Input = input; Output = []; Intcode = intcode; ProgramCompleted = false; RelativeBase = 0; }
