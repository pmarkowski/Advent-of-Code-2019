open System
open System.IO

let inputFile = "input.txt"

let stringToIntcode (textInput:string) =
    textInput.Split([|','|])
    |> Array.map int64
    |> Array.toSeq

let readFileToIntcode path =
    Path.Combine(__SOURCE_DIRECTORY__, path)
    |> File.ReadLines
    |> Seq.head
    |> stringToIntcode

let intcodeWithValueAtLocation storageLocation operationResult intcode =
    intcode
    |> Seq.mapi (fun i value ->
        if i = (int storageLocation) then operationResult
        else value)

let getValueOfDigit number digitPlace =
    let powerOfTen =  int64 (Math.Pow(10.0, (float digitPlace)))
    let nextPowerOfTen = powerOfTen * 10L
    ((number % nextPowerOfTen) - (number % powerOfTen)) / powerOfTen

let getValue (intcode:int64 seq) (parameter:int64) (parameterMode:int64) (relativeBase:int) =
    match parameterMode with
    | 0L -> Seq.item (int parameter) intcode
    | 1L -> parameter
    | 2L -> Seq.item (relativeBase + (int parameter)) intcode
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
    Intcode : int64 seq;
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
        let opcode = Seq.item intcodeComputer.InstructionPointer intcodeComputer.Intcode

        let firstParameterMode = getValueOfDigit opcode 2
        let secondParameterMode = getValueOfDigit opcode 3
        let thirdParameterMode = getValueOfDigit opcode 4

        let firstParameterValue = getValue intcodeComputer.Intcode (Seq.item (intcodeComputer.InstructionPointer + 1) intcodeComputer.Intcode) firstParameterMode intcodeComputer.RelativeBase
        let secondParameterValue = getValue intcodeComputer.Intcode (Seq.item (intcodeComputer.InstructionPointer + 2) intcodeComputer.Intcode) secondParameterMode intcodeComputer.RelativeBase

        let operationResult = operation firstParameterValue secondParameterValue
        let storageLocation = if thirdParameterMode = 0L then (Seq.item (intcodeComputer.InstructionPointer + 3) intcodeComputer.Intcode) else ((Seq.item (intcodeComputer.InstructionPointer + 3) intcodeComputer.Intcode) + (int64 intcodeComputer.RelativeBase))

        runIntcodeComputer { intcodeComputer with
                                InstructionPointer = intcodeComputer.InstructionPointer + 4;
                                Intcode = (intcodeWithValueAtLocation storageLocation operationResult intcodeComputer.Intcode) }

    // i is the operation we're executing
    // i + 1 is the position we want to store our input
    let storeInputOperation intcodeComputer =
        let inputValue = List.tryHead intcodeComputer.Input

        let opcode = Seq.item intcodeComputer.InstructionPointer intcodeComputer.Intcode

        let firstParameterMode = getValueOfDigit opcode 2
        let storageLocation = if firstParameterMode = 0L then (Seq.item (intcodeComputer.InstructionPointer + 1) intcodeComputer.Intcode) else ((Seq.item (intcodeComputer.InstructionPointer + 1) intcodeComputer.Intcode) + (int64 intcodeComputer.RelativeBase))

        match inputValue with
        | None -> { intcodeComputer with ProgramCompleted = false; }
        | Some inputValue -> runIntcodeComputer {
            intcodeComputer with
                InstructionPointer = intcodeComputer.InstructionPointer + 2;
                Input = (List.tail intcodeComputer.Input)
                Intcode = (intcodeWithValueAtLocation storageLocation inputValue intcodeComputer.Intcode) }

    // i is the operation we're executing
    // i + 1 is the position we want to output
    let outputOperation intcodeComputer =
        let opcode = (Seq.item intcodeComputer.InstructionPointer intcodeComputer.Intcode)
        let firstParameterMode = getValueOfDigit opcode 2
        let outputValue = getValue intcodeComputer.Intcode (Seq.item (intcodeComputer.InstructionPointer + 1) intcodeComputer.Intcode) firstParameterMode intcodeComputer.RelativeBase
        runIntcodeComputer { intcodeComputer with
                                InstructionPointer = intcodeComputer.InstructionPointer + 2;
                                Output = outputValue::intcodeComputer.Output; }

    let jumpOnComparisonToZero operation (intcodeComputer:IntcodeComputer) =
        let opcode = (Seq.item intcodeComputer.InstructionPointer intcodeComputer.Intcode)

        let firstParameterMode = getValueOfDigit opcode 2
        let secondParameterMode = getValueOfDigit opcode 3

        let firstParameterValue = getValue intcodeComputer.Intcode (Seq.item (intcodeComputer.InstructionPointer + 1) intcodeComputer.Intcode) firstParameterMode intcodeComputer.RelativeBase
        let secondParameterValue = getValue intcodeComputer.Intcode (Seq.item (intcodeComputer.InstructionPointer + 2) intcodeComputer.Intcode) secondParameterMode intcodeComputer.RelativeBase

        if operation firstParameterValue 0L then
            runIntcodeComputer { intcodeComputer with
                                    InstructionPointer = int secondParameterValue; }
        else
            runIntcodeComputer { intcodeComputer with
                                    InstructionPointer = (intcodeComputer.InstructionPointer + 3); }

    let relativeBaseOffsetOperation intcodeComputer =
        let opcode = (Seq.item intcodeComputer.InstructionPointer intcodeComputer.Intcode)
        let firstParameterMode = getValueOfDigit opcode 2

        let firstParameterValue = getValue intcodeComputer.Intcode (Seq.item (intcodeComputer.InstructionPointer + 1) intcodeComputer.Intcode) firstParameterMode intcodeComputer.RelativeBase

        runIntcodeComputer { intcodeComputer with
                                InstructionPointer = (intcodeComputer.InstructionPointer + 2);
                                RelativeBase = (intcodeComputer.RelativeBase + int firstParameterValue); }

    let opcode = (Seq.item intcodeComputer.InstructionPointer intcodeComputer.Intcode) % 100L
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

let runIntcode (input:int64 list) (intcode:int64 seq) : IntcodeComputer =
    runIntcodeComputer { InstructionPointer = 0; Input = input; Output = []; Intcode = Seq.append intcode (Seq.initInfinite (fun x-> 0L)); ProgramCompleted = false; RelativeBase = 0; }

let solveProblem1 inputPath =
    inputPath
    |> readFileToIntcode
    |> runIntcode [1L]
