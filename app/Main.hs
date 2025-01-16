module Main where

import Data.Char (toUpper)
import Data.List
import Data.Maybe (fromMaybe)
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
main = putStrLn "Hello, Haskell!"

