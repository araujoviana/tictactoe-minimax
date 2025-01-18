-- Based on the python implementation by Cledersonbc at https://github.com/Cledersonbc/tic-tac-toe-minimax
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use if" #-}

-- REVIEW Remove all logic that implies that the player could choose its mark

module Main where

import Data.Char (toLower)
import Data.List (maximumBy, minimumBy, transpose)
import Data.Ord (comparing)
import Text.Read (readMaybe)

-- X is the minimizing player and O is the minimizing player
data Mark = O | Empty | X deriving (Eq, Ord)

instance Read Mark where
  readsPrec _ "O" = [(O, "")]
  readsPrec _ "X" = [(X, "")]
  readsPrec _ _ = [] -- Users shouldn't be able to write Empty

instance Show Mark where
  show O = "O"
  show X = "X"
  show Empty = "_" -- Show Empty diversely

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
  putStrLn "Welcome to Tic-Tac-Toe!\nYou are X."

  let player = X
  let bot = O

  let board = replicate 3 (replicate 3 Empty) -- Empty board
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
    then do
      putStrLn $
        "Game Over! "
          ++ case map (markWins board) [player, bot] of
            [True, False] -> "The Player wins!"
            [False, True] -> "The Bot wins!"
            -- If someone ever sees this message then something is certainly wrong...
            [True, True] -> "Both win? Theres something wrong..."
            _ -> "Nobody wins! (it's a draw.)"
      putStrLn $ "\n" ++ displayBoard board
    else case first of
      True -> do
        -- Player first
        playerMovedBoard <- getPlayerMove board player -- A new board after the player moves
        botMovedBoard <- getBotMove playerMovedBoard bot -- A new board after the bot moves
        gameLoop True botMovedBoard player bot
      False -> do
        botMovedBoard <- getBotMove board bot -- A new board after the bot moves
        playerMovedBoard <- getPlayerMove botMovedBoard player -- A new board after the player moves
        gameLoop False playerMovedBoard player bot

getPlayerMove :: Board -> Mark -> IO Board
getPlayerMove board player = do
  putStrLn $ "\n" ++ displayBoard board ++ "\n\nEnter you move (1-9)"
  input <- getLine
  let pos = readMaybe input :: Maybe Int
  -- REVIEW There's three checks being made here (with two ifs!),
  -- so this function should be rewritten to be more functional(?)
  case pos of
    Just position ->
      if position > 0 && position <= 9
        then do
          -- Input is valid!
          let row = (position - 1) `div` 3
              column = (position - 1) `mod` 3
          if (board !! row !! column) == Empty
            then return (applyMove board (row, column) player)
            else do
              -- Input is in a filled tile
              putStrLn "\nTile is full! Try again."
              getPlayerMove board player
        else do
          -- Input is out of bounds
          putStrLn "\nInvalid input! Enter a number between 1 and 9."
          getPlayerMove board player
    Nothing -> do
      -- Input is not a number
      putStrLn "\nInvalid input! Enter a number between 1 and 9."
      getPlayerMove board player

displayBoard :: Board -> String
displayBoard board = unlines (map displayRow board)
  where
    -- This function will display a single row of the board
    displayRow :: [Mark] -> String
    displayRow row = unwords (map show row)

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
minimax :: Board -> Mark ->  (Position, Score)
minimax board bot
  | gameOver board = (head (emptyPositions board), evaluate board bot)
  | bot == X = maximizingMove board bot
  | bot == O = minimizingMove board bot

maximizingMove :: Board -> Mark -> (Position, Score)
maximizingMove board bot = maximumBy (comparing snd) moves
  where
    moves = [(pos, minimax (applyMove board pos bot) (opponent bot)) | pos <- emptyPositions board]

minimizingMove :: Board -> Mark -> (Position, Score)
minimizingMove board bot = minimumBy (comparing snd) moves
  where
    moves = [(pos, minimax (applyMove board pos bot) (opponent bot)) | pos <- emptyPositions board]

opponent :: Mark -> Mark
opponent X = O
opponent O = X

emptyPositions :: Board -> [Position]
emptyPositions board =
  [(row, column) | row <- [0, 1, 2], column <- [0, 1, 2], board !! row !! column == Empty]

-- TODO An entertaining explanation about this could be written...
applyMove :: Board -> Position -> Mark -> Board
applyMove board (row, column) mark =
  take row board
    ++ [ take column (board !! row)
           ++ [mark]
           ++ drop (column + 1) (board !! row)
       ]
    ++ drop (row + 1) board

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
