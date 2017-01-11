open System

module Application = 
    open TicTacToe.Implementation
    open TicTacToe.ConsoleUi

    let startGame() = 
        let api = TicTacToe.Implementation.api
        TicTacToe.ConsoleUi.startGame api

Application.startGame()
