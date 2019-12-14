open System
open System.IO

let day = int (Environment.GetCommandLineArgs().[2])

let newDirectory = Directory.CreateDirectory(Path.Combine(__SOURCE_DIRECTORY__, sprintf "Day %d" day))

let fSharpScript = File.CreateText(Path.Combine(newDirectory.FullName, (sprintf "day%d.fsx" day)))
fSharpScript.Write(@"
open System
open System.IO

let problemInputPath = Path.Combine(__SOURCE_DIRECTORY__, ""input.txt"")
let testInputPath testNumber = Path.Combine(__SOURCE_DIRECTORY__, sprintf ""test%d.txt"" testNumber)
")

File.Create(Path.Combine(newDirectory.FullName, "input.txt"))