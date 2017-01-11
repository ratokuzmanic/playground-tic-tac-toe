namespace TicTacToe

module Implementation =
    open Domain

    type GameState = {
        cells: Cell list
    }

    let allHorizontalPositions = [Left;Center;Right]
    let allVerticalPositions = [Top;Middle;Bottom]

    type Line = Line of CellPosition list

    let linesToCheck = 
        let makeHorizontalLine v = Line [for h in allHorizontalPositions -> (h,v)]
        let horizontalLines = [for v in allVerticalPositions -> makeHorizontalLine v]

        let makeVerticalLine h = Line [for v in allVerticalPositions -> (h,v)]
        let verticalLines = [for h in allHorizontalPositions -> makeVerticalLine h]


        let diagonalLine1 = Line[Left,Top; Center,Middle; Right,Bottom]
        let diagonalLine2 = Line[Left,Bottom; Center,Middle; Right,Top]

        [
            yield! horizontalLines
            yield! verticalLines
            yield  diagonalLine1
            yield  diagonalLine2
        ]

    let getDisplayInfo gameState =
        { DisplayInfo.cells = gameState.cells }

    let getCell gameState positionToFind =
        gameState.cells
        |> List.find (fun cell -> cell.position = positionToFind)

    let private updateCell newCell gameState = 
        let substituteNewCell oldCell =
            if oldCell.position = newCell.position then
                newCell
            else 
                oldCell

        let newCells = gameState.cells |> List.map substituteNewCell 

        { gameState with cells = newCells }

    let private isGameWonBy player gameState =
        let cellWasPlayedBy playerToCompare cell =
            match cell.state with
            | Played player -> player = playerToCompare
            | Empty -> false
        
        let lineIsAllSamePlayer player (Line cellPositionList) =
            cellPositionList
            |> List.map (getCell gameState)
            |> List.forall (cellWasPlayedBy player)

        linesToCheck
        |> List.exists (lineIsAllSamePlayer player)

    let private isGameTied gameState =
        let cellWasPlayed cell =
            match cell.state with
            | Played _ -> true
            | Empty -> false

        gameState.cells
        |> List.forall cellWasPlayed
        
    let private remainingMoves gameState =
        let playableCell cell = 
            match cell.state with
            | Played player -> None
            | Empty -> Some cell.position

        gameState.cells
        |> List.choose playableCell

    let otherPlayer player =
        match player with
        | PlayerX -> PlayerO
        | PlayerO -> PlayerX

    let moveResultFor player displayInfo nextMoves =
        match player with
        | PlayerX -> PlayerXToMove (displayInfo, nextMoves)
        | PlayerO -> PlayerOToMove (displayInfo, nextMoves)

    let makeNextMoveInfo f player gameState cellPosition =
        let capability() = f player cellPosition gameState
        { positionToPlay = cellPosition; capability = capability }

    let makeMoveResultWithCapabilities f player gameState cellPositionList =
        let displayInfo = getDisplayInfo gameState
        cellPositionList
        |> List.map (makeNextMoveInfo f player gameState)
        |> moveResultFor player displayInfo

    let rec playerMove player cellPosition gameState =
        let newCell = { position = cellPosition; state = Played player }
        let newGameState = gameState |> updateCell newCell
        let displayInfo = getDisplayInfo newGameState

        if newGameState |> isGameWonBy player then
            GameWon (displayInfo, player) 
        elif newGameState |> isGameTied then
            GameTied displayInfo 
        else
            let otherPlayer = otherPlayer player 
            let moveResult = 
                newGameState 
                |> remainingMoves
                |> makeMoveResultWithCapabilities playerMove otherPlayer newGameState
            moveResult

    let newGame() =
        let allPositions = [
            for h in allHorizontalPositions do
            for v in allVerticalPositions do 
                yield (h,v)
        ]

        let emptyCells = 
            allPositions
            |> List.map (fun position -> { position = position; state = Empty })
        
        let gameState = { cells = emptyCells }

        let moveResult = 
            allPositions
            |> makeMoveResultWithCapabilities playerMove PlayerX gameState

        moveResult

    let api = {
        newGame = newGame
    }