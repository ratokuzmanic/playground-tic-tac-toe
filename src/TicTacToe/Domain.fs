namespace TicTacToe 

module Domain =
    type Player = PlayerX | PlayerO
    
    type HorizontalPosition = Left | Center | Right
    type VerticalPosition = Top | Middle | Bottom
    type CellPosition = HorizontalPosition * VerticalPosition

    type CellState = 
        | Played of Player
        | Empty

    type Cell = {
        position: CellPosition
        state: CellState
    }

    type DisplayInfo = {
        cells: Cell list 
    }

    type Move = unit -> MoveResult
    and NextPossibleMove = {
        positionToPlay: CellPosition
        capability: Move
    }
    and MoveResult = 
        | PlayerXToMove of DisplayInfo * (NextPossibleMove list)
        | PlayerOToMove of DisplayInfo * (NextPossibleMove list)
        | GameWon of DisplayInfo * Player
        | GameTied of DisplayInfo

    type Api = {
        newGame: Move
    }
