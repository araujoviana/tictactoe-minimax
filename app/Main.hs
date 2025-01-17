module Main where

import Data.Char (toLower)
import Data.List ()
import Data.Maybe (fromMaybe, isNothing)
import Text.Read (readMaybe)

data Mark = X | O deriving (Show, Read, Eq, Ord)

-- Tic-Tac-Toe Board
---------------
-- [[O,X,X], --
--  [X, ,O], --
--  [X,O, ]] --
---------------
type Board = [[Maybe Mark]] -- Any tile could be empty

main :: IO ()
main = do
  putStrLn "Welcome to Tic-Tac-Toe!\nAre you X or O?"
  input <- getLine

  let player = fromMaybe X $ readMaybe input
  let bot = case player of -- Your opponent has the opposing mark
        X -> O
        O -> X

  putStrLn $ "You are " ++ show player

  -- The empty board
  let board = replicate 3 (replicate 3 Nothing) :: Board

  putStrLn "Do you want to play first? [y/n]:"
  choice <- getLine
  case map toLower choice of
    "y" -> gameLoop True board player bot
    "n" -> gameLoop False board player bot
    _ -> do
      putStrLn "Invalid choice, enter only y or n"
      main

gameLoop :: Bool -> Board -> Mark -> Mark -> IO ()
gameLoop first player bot = do
  if emptyCells > 0 and (not $ gameOver board)
    then
      ( if first
          then
            ( do
                botTurn board player bot
                humanTurn board player bot
                gameLoop first board player bot
            )
          else
            ( do
                humanTurn board player bot
                botTurn board player bot
                gameLoop first board player bot
            )
      )
    else undefined -- TODO Game over here!
  where
    emptyCells = length . concatMap (filter isNothing) board

gameOver :: Board -> Bool
gameOver board = undefined
