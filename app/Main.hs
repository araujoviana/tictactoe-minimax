-- Based on the python implementation Cledersonbc at https://github.com/Cledersonbc/tic-tac-toe-minimax
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use if" #-}

module Main where

import Control.Monad
import Data.Char (toLower)
import Data.List (transpose)
import Data.Maybe (fromMaybe, isNothing)
import Text.Read (readMaybe)

-- X is the minimizing player and O is the minimizing player
data Mark = X | Empty | O deriving (Show, Read, Eq, Ord)

-- Tic-Tac-Toe Board
---------------
-- [[O,X,X], --
--  [X, ,O], --
--  [X,O, ]] --
---------------
type Board = [[Mark]] -- Any tile could be empty

type Score = Int

type Position = (Int, Int)

main :: IO ()
main = do
  putStrLn "Welcome to Tic-Tac-Toe!\nAre you X or O?"
  input <- getLine

  let player = fromMaybe X $ readMaybe input -- TODO verify is input is Empty
  let bot = case player of -- Your opponent has the opposing mark
        X -> O
        O -> X

  putStrLn $ "You are " ++ show player

  -- The empty board
  let board = replicate 3 (replicate 3 Empty) :: Board

  putStrLn "Do you want to play first? [y/n]:"
  choice <- getLine
  case map toLower choice of
    "y" -> gameLoop True board player bot
    "n" -> gameLoop False board player bot
    _ -> do
      putStrLn "Invalid choice, enter only y or n"
      main

gameLoop :: Bool -> Board -> Mark -> Mark -> IO ()
gameLoop first board player bot =
  if gameOver board
    then undefined -- TODO Winner dialogue
    else case first of
      True -> do
        -- Player first
        let playerMovedBoard = getPlayerMove board player -- A new board after the player moves
        -- let botMovedBoard = getBotMove playerMovedBoard bot -- A new board after the bot moves, also TODO thats an IO Board!
        -- TODO here you'd write whatever turns an IO Board into a board and call game loop again
        putStrLn "Balls"
      False -> do
        let botMovedBoard = getBotMove board bot -- A new board after the bot moves
        -- let playerMovedBoard = getPlayerMove botMovedBoard player -- A new board after the player moves, also TODO thats an IO Board!
        -- TODO here you'd write whatever turns an IO Board into a board and call game loop again
        putStrLn "Balls"

getPlayerMove :: Board -> Mark -> IO Board
getPlayerMove board player = do
  putStrLn $ "\n" ++ show board ++ "\n\nEnter you move (1-9)"
  input <- getLine
  let pos = readMaybe input :: Maybe Int
  case pos of
    Just position -> do
      let row = (position - 1) `div` 3
          column = (position - 1) `mod` 3
      if (board !! row !! column) == Empty
        then return (updateBoard board row column player)
        else do
          putStrLn "Tile is full! Try again."
          getPlayerMove board player
    Nothing -> do
      putStrLn "Invalid input! Enter a number between 1 and 9."
      getPlayerMove board player

getBotMove :: Board -> Mark -> IO Board
getBotMove board bot = undefined -- TODO

updateBoard :: Board -> Int -> Int -> Mark -> Board
updateBoard board row column mark =
  -- REVIEW Unreadable?
  take row board
    ++ [ take column (board !! row)
           ++ [mark]
           ++ drop (column + 1) (board !! row)
       ]
    ++ drop (row + 1) board

gameOver :: Board -> Bool
gameOver board = any ($ board) [horizontalCheck, verticalCheck, diagonalCheck]

-- Checks if there's a horizontal win on the board by verifying any row has the same non-empty piece
horizontalCheck :: Board -> Bool
horizontalCheck = any (\b -> all (/= Empty) b && all (== head b) b)

-- Checks for a winning condition in the vertical direction on a Board by transposing and checking horizontally
verticalCheck :: Board -> Bool
verticalCheck = horizontalCheck . transpose

-- Checks if a winning condition exists on either of the diagonals of a board
diagonalCheck :: Board -> Bool
diagonalCheck board =
  let firstDiagonalCheck b =
        horizontalCheck [[b !! i !! i | i <- [0 .. length b - 1]]]
      secondDiagonalCheck b =
        horizontalCheck [[b !! i !! (length b - 1 - i) | i <- [0 .. length b - 1]]]
   in firstDiagonalCheck board || secondDiagonalCheck board

-- This is what matters, the minimax!
minimax :: Board -> Mark -> Score
minimax board bot = undefined

emptyPositions :: Board -> [Position]
emptyPositions board =
  [(row, column) | row <- [0, 1, 2], column <- [0, 1, 2], board !! row !! column == Empty]


-- Evaluate, interprets a board and checks if its beneficial to the bot.
evaluate :: Board -> Mark -> Int
evaluate board bot
  | horizontalCheck board = if checkWinner board bot then 1 else -1
  | verticalCheck board = if checkWinner board bot then 1 else -1
  | diagonalCheck board = if checkWinner board bot then 1 else -1
  | otherwise = 0
  where
    checkWinner b mark =
      any (all (== mark)) b
        || any (all (== mark)) (transpose b)
        || checkDiagonals b mark
    checkDiagonals b mark =
      let n = length b
          -- REVIEW Too complicated, maybe make it a function?
          mainDiagonal = [b !! i !! i | i <- [0 .. n - 1]]
          antiDiagonal = [b !! i !! (n - 1 - i) | i <- [0 .. n - 1]]
       in all (== mark) mainDiagonal || all (== mark) antiDiagonal

-- NOTE TESTING BOARDS FOR REPL

-- All rows are the same (horizontal win for X)
board1 :: Board
board1 =
  [ [X, X, X],
    [X, X, X],
    [X, X, X]
  ]

-- Mixed rows (no horizontal win)
board2 :: Board
board2 =
  [ [X, X, O],
    [X, O, X],
    [O, X, O]
  ]

-- One row is uniform, but others aren't (no horizontal win)
board3 :: Board
board3 =
  [ [X, X, X],
    [O, Empty, O],
    [X, O, X]
  ]

-- All rows are uniform, but with different Marks (no horizontal win)
board4 :: Board
board4 =
  [ [X, X, X],
    [O, O, O],
    [Empty, Empty, Empty]
  ]

-- Empty board (no horizontal win)
board5 :: Board
board5 =
  [ [Empty, Empty, Empty],
    [Empty, Empty, Empty],
    [Empty, Empty, Empty]
  ]

-- Horizontal win for O
board6 :: Board
board6 =
  [ [O, O, O],
    [O, O, O],
    [O, O, O]
  ]

boards = [board1, board2, board3, board4, board5, board6]
