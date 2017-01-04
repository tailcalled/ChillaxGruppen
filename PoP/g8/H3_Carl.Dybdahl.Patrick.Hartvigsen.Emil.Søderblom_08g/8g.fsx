//#############
//Opgave 8g - Mastermind
//#############

//Program types
type codeColor =
    Red | Green | Yellow | Purple | White | Black
type code = codeColor list
type answer = int * int
type board = ( code * answer ) list
type player = Human | Computer

/// <summary>
///  Produce a list of all possible combination of code(code have 4 elements).
/// </summary>
/// <example>
///   <code>
///     let s = makeCodes
///   </code>
/// </example>
/// <returns>code with length of 4.</returns>
let makeCodes =
    let colors = [Black; White; Purple; Yellow; Green; Red]
    let rec codesOfLength = function
        | 0 -> [[]]
        | n ->
            let subCodes = codesOfLength (n - 1)
            colors |> List.collect (fun col ->
                subCodes  |> List.map ((fun x xs -> x :: xs) col))
    codesOfLength 4

/// <summary>
///  Converts a char-list to code. (with chars 'r','g,'y','p','w,'b')
/// </summary>
/// <remarks> All other chars than r','g,'y','p' and 'w' will give Black
/// </remarks>
/// <example>
///   <code>
///     charListToCode (List.ofSeq "rrrr")
///   </code>
/// </example>
/// <param name="code"> A list of chars.</param>
/// <returns> Returns code, the example above til return [Red;Red;Red;Red].
/// </returns>
let rec charListToCode (code : char list) : code =
    match code with
    | [] -> []
    | x :: xs when x = 'r' -> Red :: charListToCode xs
    | x :: xs when x = 'g' -> Green :: charListToCode xs
    | x :: xs when x = 'y' -> Yellow :: charListToCode xs
    | x :: xs when x = 'p' -> Purple :: charListToCode xs
    | x :: xs when x = 'w' -> White :: charListToCode xs
    | x :: xs (*   'b'  *) -> Black :: charListToCode xs

/// <summary>
///  Checks if a string is the length of 4
///  and only contaions chars:'r','g,'y','p','w,'b'.
/// </summary>
/// <example>
///   <code>
///    checkStringCode "rrrr"
///   </code>
/// </example>
/// <param name="code"> A string. </param>
/// <returns>
///  A boolean, if string is valid, it returns true else false.
///  The example above wil return true.
/// </returns>
let checkStringCode (code : string) : bool =
    let checkChars (str : string) =
        str |> String.forall (fun ch -> "rgypwb" |> String.exists ((=) ch))
    match code.Length with
    | 4 -> checkChars code
    | _ -> false

// This variable is used as a hook, for testing funktions with user inputs in 8gTests.fsx.
let mutable readUserLine = fun () -> System.Console.ReadLine ()

/// <summary>
///  Gets input from user as a string. And checks if string is valid
///  with a format function. If string is valid then return the string
///  else gets new input from user and checks if valid.
/// </summary>
/// <example>
///   <code>
///    getUserInput playerFormat
///   </code>
/// </example>
/// <param name="format"> A format function for what is allowed to pass.</param>
/// <returns> Returns option type with string. </returns>
let rec getUserInput(format : string -> 'a option) =
    printf "> "
    match format <| readUserLine () with
    | None ->
        printfn "Invalid input."
        getUserInput format
    | Some x -> x

/// <summary>
///  Format for checking input when asking for who is playing.
///  Checks if input is "1","2","3" or "4".
/// </summary>
/// <example>
///   <code>
///     getUserInput playerFormat
///   </code>
/// </example>
/// <param name=""> A string.</param>
/// <returns>
///  Returns option type, if string not valid than it returns None
///  else it returns players.
/// </returns>
let playerFormat = function
    | "1" -> Some (Human, Computer)
    | "2" -> Some (Human, Human)
    | "3" -> Some (Computer, Human)
    | "4" -> Some (Computer, Computer)
    | _   -> None

/// <summary>
///  Format for checking input when asking for a for a string-code.
///  checks if input is allowed with checkStringCode.
/// </summary>
/// <example>
///   <code>
///     getUserInput codeFormat
///   </code>
/// </example>
/// <param name=""> A string.</param>
/// <returns>
///  Returns option type, if string not valid than it returns None
///  else it returns code.
/// </returns>
let codeFormat = function
    | x when x |> checkStringCode -> Some (List.ofSeq x |> charListToCode)
    | _ -> None

/// <summary>
///  makes a code of length 4. If player is Human then it ask user for input.
///  else if player is Computer produce random code.
/// </summary>
/// <example>
///   <code>
///     makeCode Computer
///   </code>
/// </example>
/// <param name="p"> A player type.</param>
/// <returns> Returns code </returns>
let makeCode (p : player) : code =
    match p with
    | Human ->
        printfn "Choose color code with length of 4 with: \
        r=Red, g=Green, y=Yellow, p=Purple, w=White, b=Black."
        getUserInput codeFormat
    | Computer ->
        let r = System.Random ()
        makeCodes.[(r.Next (0,1297))]

/// <summary>
///  Validate your guess with the correct code.
/// </summary>
/// <example>
///   <code>
///     validate [Red;Red;Red;Red] [Red;Red;Red;Green]
///   </code>
/// </example>
/// <param name="c"> The correct code.</param>
/// <param name="g"> Guess from player.</param>
/// <returns>
///  Returns a tuple of white and black pins.
///  White corrrect color, black correct color and position.
/// </returns>
let validate (c : code) (g : code) : answer =
    let codeColourLst = [Red; Green; Yellow; Purple; White; Black]
    let white = List.sum (List.map (fun col ->
        let countG = List.length (List.filter ((=) col) g)
        let countC = List.length (List.filter ((=) col) c)
        min countG countC
        ) codeColourLst)
    let black = List.sum (List.map (fun (colC,colG) ->
        if colC = colG then 1 else 0) (List.zip c g))
    (white-black,black)

/// <summary>
///  Determines the best guess out of possible guesses.
/// </summary>
/// <example>
///   <code>
///     s |> List.maxBy (guessQuality s)
///   </code>
/// </example>
/// <param name="option"> Possible code guesses. </param>
/// <param name="guess">  Checks quality of guess. </param>
/// <returns>
///  Returns a float, a higher number means a better guess.
/// </returns>
let guessQuality (options : code list) (guess : code) : float =
    let answers: answer list = options |> List.map (validate guess)
    let allAnswers: answer list =
        [0 .. 4] |> List.collect (fun w ->
            [0 .. 4 - w] |> List.map (fun b ->
                (w, b)))
    let initCounts = allAnswers |> List.map (fun x -> (x, 0)) |> Map.ofList
    let counts = answers |> (List.fold (fun q a ->
        let count = q |> Map.find a |> (+) 1
        Map.add a count q
    ) initCounts)
    let eps = 1.0
    counts |> Map.toList |> List.map(snd >> float >> (+) eps >> log) |> List.sum

/// <summary>
///  Reduce possible guesses with use of previously
///  guesses and answers, and returns a guess.
/// </summary>
/// <example>
///   <code>
///     guessBot [((0,3),[Red;Red;Red;Red]);((0,3),[Red;Red;Red;Green])]
///   </code>
/// </example>
/// <param name="b1"> The board, previously guesse and answers.</param>
/// <returns> Returns a guess as type code. </returns>
let guessBot (b1 : board) : code =
    match b1 with
    | [] -> [Red;Red;Green;Green]
    | _  ->
        let s = List.foldBack (fun (c, (w, b)) ->
            List.filter (fun x -> (validate x c) = (w,b))) b1 makeCodes
        s |> List.maxBy (guessQuality s)

/// <summary>
///  Returns a guess of code. If player is Human gets input from user.
/// If player is Computer it will 'calculate' a guess of code.
/// </summary>
/// <example>
///   <code>
///     guess Human [((0,3),[Red;Red;Red;Red]);((0,3),[Red;Red;Red;Green])]
///   </code>
/// </example>
/// <param name="p"> Player type.</param>
/// <param name="b"> The board, previously guesse and answers.</param>
/// <returns> Returns a guess as type code. </returns>
let guess (p : player) (b : board) : code =
    match p with
    | Human ->
        printfn "Previous guesses: %A" b
        printfn "Try a guess on the color code: \
        r=Red, g=Green, y=Yellow, p=Purple, w=White, b=Black."
        getUserInput codeFormat
    | Computer -> guessBot b

/// <summary>
///  Initialize game variabels, player types and the correct-code.
/// </summary>
/// <example>
///   <code>
///     gameInit()
///   </code>
/// </example>
/// <returns> Returns player types and code </returns>
let gameInit() =
    printfn "Welcome to Mastermind SUPER TEXT 0.3x Supreme digital edition."
    printfn "The guesser have 30 tries to guess a color-code made by the coder."
    printfn "Choose how to play (guesser vs coder):
    1: Human vs Computer
    2: Human vs Human
    3: Computer vs Human
    4: Computer vs Computer"
    let p1, p2 = getUserInput playerFormat
    (p1,p2, makeCode p2)

/// <summary>
///  Starts the game, and the game loop.
/// </summary>
/// <example>
///   <code>
///     gameStart()
///   </code>
/// </example>
/// <returns> Returns unit (prints information out). </returns>
let gameStart() =
    let (p1,p2,colorCode) = gameInit()
    let maxTurns = 30
    printfn "\nGame starts!\n"
    let rec gameLoop (B : board) (turns : int) =
        let G = guess p1 B
        let A = validate colorCode G
        match A with
        | (0,4) -> printfn "\nYou won in %d turns! :D" turns
        | _ when turns <= 30 ->
            printfn "\nNext turn"
            gameLoop ((G,A) :: B) (turns + 1)
        | _ -> printfn "\nYou have used your 30 turns, you lose"
    gameLoop [] 0

[<EntryPoint>]
let main args =
    gameStart()
    0