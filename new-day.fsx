open System
open System.IO

let day = int (Environment.GetCommandLineArgs().[2])

let newDirectory = Directory.CreateDirectory(Path.Combine(__SOURCE_DIRECTORY__, sprintf "Day %d" day))

let fSharpScript = File.Create(Path.Combine(newDirectory.FullName, (sprintf "day%d.fsx" day)))
File.Create(Path.Combine(newDirectory.FullName, "input.txt"))