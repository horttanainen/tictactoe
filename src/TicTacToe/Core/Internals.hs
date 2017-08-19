module TicTacToe.Core.Internals
  ( module TicTacToe.Core.Internals
  , module TicTacToe.Domain
  , module Data.Maybe
  ) where
  
import Data.List (transpose)
import Data.List.Split (chunksOf)
import Safe (headMay)
import Control.Monad (join)
import Data.Maybe

import TicTacToe.Domain

moveNoCheck :: Board -> Move -> CellPos -> Board
moveNoCheck b m pos = before ++ ( (Cell pos (Just m)) : after)
  where (before,(_:after)) = splitAt pos b

legal :: Board -> Move -> CellPos -> Bool
legal b m pos = 
  isInsideBoard && isPlayersTurn && cellIsVacant
    where
      isPlayersTurn       = turn b == Just m
      cellIsVacant        = cellStateAt b pos == Nothing
      isInsideBoard       = pos >= 0 && pos < length b

turn :: Board -> Maybe Player
turn b
  | boardState == Unfinished  = if even nMoves then Just Cross else Just Nought
  | otherwise                 = Nothing
    where
      boardState = state b Cross
      nMoves = numberOfMoves b

checkForStrikeD :: Board -> Maybe Player
checkForStrikeD b = checkForStrikeH $ diag1 ++ diag2
  where
    rows  = boardToRows b
    diag1 = zipWith (!!) rows [0..]
    diag2 = zipWith (!!) (reverse <$> rows) [0..]

checkForStrikeV :: Board -> Maybe Player
checkForStrikeV = checkForStrikeH . concat . transpose . boardToRows

checkForStrikeH :: Board -> Maybe Player
checkForStrikeH b =
  let strikes = filter isJust $ checkForStrikeRow <$> boardToRows b
  in join $ headMay strikes

checkForStrikeRow :: [Cell] -> Maybe Player
checkForStrikeRow row
  | and $ ( == head rowOfMoves) <$> (tail rowOfMoves) = head rowOfMoves
  | otherwise                                         = Nothing
    where 
      rowOfMoves = map cellState row

boardToRows :: Board -> [[Cell]]
boardToRows = chunksOf 3

numberOfMoves :: Board -> Int
numberOfMoves = length . (filter (isJust . cellState))

cellStateAt :: Board -> CellPos -> CellState
cellStateAt b pos = cellState specifiedCell
  where (_,(specifiedCell:_)) = splitAt pos b
