open System

module ConsoleApplication = 
    open TicTacToe.Implementation
    open TicTacToe.ConsoleUi

    let startGame() = 
        let api = TicTacToe.Implementation.api
        TicTacToe.ConsoleUi.startGame api

ConsoleApplication.startGame()