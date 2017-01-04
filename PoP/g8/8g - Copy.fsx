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

let makeCodes =
    let colors = [Black; White; Purple; Yellow; Green; Red]
    let rec codesOfLength = function
        | 0 -> [[]]
        | n ->
            let subCodes = codesOfLength (n - 1)
            colors |> List.collect (fun col ->
                subCodes |> List.map ((fun x xs -> x :: xs) col))
    codesOfLength 4

let rec charListToCode (code : char list) : code =
    match code with
    | [] -> []
    | x :: xs when x = 'r' -> Red :: charListToCode xs
    | x :: xs when x = 'g' -> Green :: charListToCode xs
    | x :: xs when x = 'y' -> Yellow :: charListToCode xs
    | x :: xs when x = 'p' -> Purple :: charListToCode xs
    | x :: xs when x = 'w' -> White :: charListToCode xs
    | x :: xs (*   'b'  *) -> Black :: charListToCode xs

let checkStringCode (code : string) : bool =
    let checkChars (str : string) =
        str |> String.forall (fun ch -> "rgypwb" |> String.exists ((=) ch))
    match code.Length with
    | 4 -> checkChars code
    | _ -> false

let rec getUserInput(format : string -> 'a option) =
    printf "> "
    match format <| System.Console.ReadLine () with
    | None ->
        printfn "Invalid input."
        getUserInput format
    | Some x -> x

let playerFormat = function
    | "1" -> Some (Human, Computer)
    | "2" -> Some (Human, Human)
    | "3" -> Some (Computer, Human)
    | "4" -> Some (Computer, Computer)
    | _   -> None

let codeFormat = function
    | x when x |> checkStringCode -> Some (List.ofSeq x |> charListToCode)
    | _ -> None

let makeCode (p : player) : code =
    match p with
    | Human ->
        printfn "Choose color code with length of 4 with: \
        r=Red, g=Green, y=Yellow, p=Purple, w=White, b=Black."
        getUserInput codeFormat
    | Computer ->
        let r = System.Random ()
        makeCodes.[(r.Next (0,1297))]

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

let guessBot (b1 : board) : code =
    match b1 with
    | [] -> [Red;Red;Green;Green]
    | _  ->
        let s = List.foldBack (fun (c, (w, b)) ->
            List.filter (fun x -> (validate x c) = (w,b))) b1 makeCodes
        s |> List.maxBy (guessQuality s)

let guess (p : player) (b : board) : code =
    match p with
    | Human ->
        printfn "Previous guesses: %A" b
        printfn "Try a guess on the color code: \
        r=Red, g=Green, y=Yellow, p=Purple, w=White, b=Black."
        getUserInput codeFormat
    | Computer -> guessBot b

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

let gameStart() =
    let (p1,p2,colorCode) = gameInit()
    let maxTurns = 30
    printfn "\nGame starts!\n"
    (*mainLoop*)
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
    (*end*)

gameStart()