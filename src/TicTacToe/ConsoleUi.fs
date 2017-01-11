namespace TicTacToe
open System

module ConsoleUi =
    open TicTacToe.Domain

    type UserAction<'a> = 
        | ContinuePlay of 'a
        | ExitGame

    let displayNextMoves nextMoves = 
        nextMoves 
        |> List.iteri (fun i moveInfo -> printfn "%i) %A" i moveInfo)

    let getCapability selectedIndex nextMoves =
        if selectedIndex < List.length nextMoves then
            let move = List.nth nextMoves selectedIndex
            Some move.capability
        else
            None

    let processMoveIndex inputString availableMoves processInputAgain =
        match Int32.TryParse inputString with
        | true, inputIndex -> 
            match getCapability inputIndex availableMoves with
            | Some capability ->
                let moveResult = capability()
                ContinuePlay moveResult
            | None ->
                printfn "No move found with that index. Try again."
                processInputAgain()
        | false, _ ->
            printfn "Please enter an int corresponding to a displayed move"
            processInputAgain()

    let rec processInput availableCapabilities = 
        let processInputAgain() =
            processInput availableCapabilities

        printfn "Enter an int corresponding to a displayed move or q to quit"
        let inputString = Console.ReadLine()
        match inputString with
        | "q" -> ExitGame
        | _ -> processMoveIndex inputString availableCapabilities processInputAgain

    let displayCells displayInfo =
        let cells = displayInfo.cells
        let cellToString cell =
            match cell.state with
            | Empty -> "-"
            | Played player ->
                match player with
                | PlayerO -> "O"
                | PlayerX -> "X"
        
        let printCells cells =
            cells
            |> List.map cellToString
            |> List.reduce (fun s1 s2 -> s1 + "|" + s2)
            |> printfn "|%s|"

        let topCells = 
            cells |> List.filter (fun cell -> snd cell.position = Top)
        let middleCells =
            cells |> List.filter (fun cell -> snd cell.position = Middle)
        let bottomCells =
            cells |> List.filter (fun cell -> snd cell.position = Bottom)

        printCells topCells
        printCells middleCells
        printCells bottomCells

    let rec askToPlayAgain api = 
        printfn "Would you like to play again (y/n)?"
        match Console.ReadLine() with
        | "y" -> ContinuePlay (api.newGame())
        | "n" -> ExitGame
        | _ -> askToPlayAgain api

    let rec gameLoop api userAction =
        printfn "\n------------------------------\n"

        match userAction with
        | ExitGame ->
            printfn "Exiting game."
        | ContinuePlay moveResult ->
            match moveResult with
            | GameTied displayInfo ->
                displayInfo |> displayCells
                printfn "Game over - Tie"
                let nextUserAction = askToPlayAgain api
                gameLoop api nextUserAction
            | GameWon (displayInfo, player) ->
                displayInfo |> displayCells
                printfn "Game over - Winner: %A" player
                let nextUserAction = askToPlayAgain api
                gameLoop api nextUserAction
            | PlayerOToMove (displayInfo, nextMoves) ->
                displayInfo |> displayCells
                printfn "Player O to move"
                displayNextMoves nextMoves
                let newResult = processInput nextMoves
                gameLoop api newResult
            | PlayerXToMove (displayInfo, nextMoves) ->
                displayInfo |> displayCells
                printfn "Player X to move"
                displayNextMoves nextMoves
                let newResult = processInput nextMoves
                gameLoop api newResult
    
    let startGame api =
        let userAction = ContinuePlay (api.newGame())
        gameLoop api userAction 