//#####################################################
// Test for makeCode, guess and validate.
//#####################################################

// Testing if makeCode() returns correct type-code from string and computer produce a random code.
let testMakeCode() =
    let test (input : string) (expected : code) : unit =
        readUserLine <- fun () -> input
        printfn "User Writes: %A as input" input
        let result = makeCode Human
        if result = expected then
            printfn "[ OK ] makeCode Human %A = %A" input expected
        else
            printfn "[FAIL] makeCode Human %A = %A != %A" input result expected
    test "rbgp" [Red;Black;Green;Purple]
    test "yyww" [Yellow;Yellow;White;White]
    printfn ""
    printfn "makeCode Computer, makes Random output(code): %A" (makeCode Computer)
    printfn "makeCode Computer, makes Random output(code): %A" (makeCode Computer)

// Testing if guess() returns correct type-code from string and computer returns a code.
let testGuess() =
    let test (input : string) (input2 : board) (expected : code) : unit =
        readUserLine <- fun () -> input
        printfn "User Writes: %A as input" input
        let result = guess Human input2
        if result = expected then
            printfn "[ OK ] makeCode %A = %A" input expected
        else
            printfn "[FAIL] makeCode %A = %A != %A" input result expected
    test "rbgp" [] [Red;Black;Green;Purple]
    test "yyww" [([Red;Red;Red;Red],(1,1))] [Yellow;Yellow;White;White]
    printfn ""
    printfn "guess Computer, gives a possible guess: %A" (guess Computer [])
    printfn "guess Computer, gives a possible guess: %A"
        (guess Computer [([Red;Red;Green;Green],(0,0))])

// Testing if validate returns correct black and white pins.
let testValidate() =
    let test (input : code) (input2 : code) (expected : answer) : unit =
        let result = validate input input2
        if result = expected then
            printfn "[ OK ] validate %A %A = %A" input input2 expected
        else
            printfn "[FAIL] validate %A %A = %A != %A"
                input input2 result expected
    test [Red;Red;Red;Red] [Red;Red;Red;Red] (0,4)
    test [Black;Red;Purple;Green] [Red;Black;Red;Red] (2,0)
    test [Black;Red;Purple;Green] [Red;Black;Purple;Green] (2,2)

testMakeCode()
printfn "\n"
testGuess()
printfn "\n"
testValidate()
printfn "\n"

//####################################################################
//Test for full game. Average turns for Computer to guess the code.
//####################################################################

/// <summary>
///  Calculates the average turns i takes for Computer to guess a code.Cons
/// </summary>
/// <example>
///   <code>
///     //printfn "%A" average
///   </code>
/// </example>
/// <returns> Returns float. </returns>
let average() = makeCodes |> List.averageBy (fun secret ->
    let maxTurns = 30
    let mutable turns = 0
    let mutable res = 0
    let mutable B = []
    printfn "game begin"
    while (maxTurns > turns) do
        turns <- turns + 1
        let G = guess Computer B
        let A = validate secret G
        if A = (0,4) then
            printfn "you win in %A turns! :D" turns
            res <- turns
            turns <- 30
        else
            B <- (G,A) :: B
    float res
)

//printfn "%A" (average())


// Average turns for computer without advance-guess: 5.021
// Average turns for computer with advance-guess where eps is 0.1: 4.457

// Average turns for computer with advance-guess where eps is 1.0: 4.455,
// worst case is 6 turns and best case is 1 turn.

// Average turns for computer with advance-guess where eps is 0.01: 4.458
// Average turns for computer with advance-guess where eps is 10: 4.456
// Average turns for computer with advance-guess where eps is 5.0: 4.456
// Average turns for computer with advance-guess where eps is 2.0: 4.455