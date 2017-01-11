namespace TicTacToe
open System

module ConsoleUi =
    open TicTacToe.Domain

    type UserAction<'a> = 
        | ContinuePlay of 'a
        | ExitGame

    let displayNextMoves nextMoves = 
        nextMoves 
        |> List.iteri (fun i moveInfo -> printfn "%i) %A" i moveInfo.positionToPlay)

    let getCapability selectedIndex nextMoves =
        if selectedIndex < List.length nextMoves then
            let move = nextMoves.[selectedIndex]
            Some move.capability
        else
            None

    let processMoveIndex inputString availableCapabilities processInputAgain =
        match Int32.TryParse inputString with
        | (true, inputIndex) -> 
            match getCapability inputIndex availableCapabilities with
            | Some capability ->
                let moveResult = capability()
                ContinuePlay moveResult
            | None ->
                printfn "No move found with that index. Please try again."
                processInputAgain()
        | (false, _) ->
            printfn "Please enter an integer that represents a valid move index."
            processInputAgain()

    let rec processInput availableCapabilities = 
        let processInputAgain() =
            processInput availableCapabilities

        printfn "Enter an integer corresponding to a displayed move or q to quit."
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
                | PlayerX -> "X"
                | PlayerO -> "O"
        
        let printCells cells =
            cells
            |> List.map cellToString
            |> List.reduce (fun s1 s2 -> s1 + "|" + s2)
            |> printfn "|%s|"

        let getRowOfCells verticalPosition =
            cells |> List.filter (fun cell -> snd cell.position = verticalPosition)

        printCells (getRowOfCells Top)
        printCells (getRowOfCells Middle)
        printCells (getRowOfCells Bottom)

    let rec askToPlayAgain api = 
        printfn "Would you like to play again (y/n)?"
        match Console.ReadLine() with
        | "y" -> ContinuePlay (api.newGame())
        | "n" -> ExitGame
        | _ -> askToPlayAgain api

    let rec gameLoop api userAction =
        match userAction with
        | ExitGame ->
            printfn "Exiting game..."
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
