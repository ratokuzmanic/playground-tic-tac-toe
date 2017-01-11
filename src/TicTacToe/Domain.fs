namespace TicTacToe 

module Domain =
    type Player = 
        | PlayerX
        | PlayerO
    
    type HorizontalPosition = Left | Center | Right
    type VerticalPosition   = Top  | Middle | Bottom
    type CellPosition       = HorizontalPosition * VerticalPosition

    type CellState = 
        | Played of Player
        | Empty

    type Cell = {
        position: CellPosition
        state:    CellState
    }

    type DisplayInfo = {
        cells: Cell list 
    }

    type MoveCapability = unit -> MoveResult
    and NextMoveInfo = {
        positionToPlay: CellPosition
        capability:     MoveCapability
    }
    and MoveResult = 
        | PlayerXToMove of DisplayInfo * NextMoveInfo list
        | PlayerOToMove of DisplayInfo * NextMoveInfo list
        | GameWon       of DisplayInfo * Player
        | GameTied      of DisplayInfo

    type TicTacToeApi = {
        newGame: MoveCapability
    }
