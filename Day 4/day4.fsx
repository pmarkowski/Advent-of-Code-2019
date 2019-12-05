open System
open System.Text.RegularExpressions

let lowerBound = 137683
let upperBound = 596253

let digitsNotDecreasing input =
    let password = string input
    Regex.IsMatch(password, @"^1*2*3*4*5*6*7*8*9*$")

let digitsContainDouble input =
    let password = string input
    Regex.IsMatch(password, @"^[0-9]*(11|22|33|44|55|66|77|88|99)+[0-9]*$")

let isValidPassword input =
    let password = string input
    password.Length = 6 &&
    digitsNotDecreasing password &&
    digitsContainDouble password

let rec findAllValidPasswordsBetween lowerBound upperBound =
    if lowerBound = upperBound then 0
    else if isValidPassword lowerBound then (+) 1 (findAllValidPasswordsBetween (lowerBound + 1) upperBound)
    else findAllValidPasswordsBetween (lowerBound + 1) upperBound
