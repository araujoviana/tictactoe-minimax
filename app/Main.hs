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
gameOver board = undefined
