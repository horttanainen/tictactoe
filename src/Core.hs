module Core where

import Data.List.Split (chunksOf)
import Data.List (transpose)
import Data.Maybe
import Control.Monad (join)
import Safe (headMay)

data Move = Nought | Cross deriving (Eq, Ord)

type Player = Move

type CellState = Maybe Move

data Cell = Cell {
  cellPos :: CellPos,
  cellState :: CellState
} deriving (Eq, Ord)

boardWidth  = 3
numberOfCells = boardWidth ^ 2

type Board = [Cell]

type Row = Int
type Col = Int
type CellPos = Int

type Run = [Board]

data Result = Win | Loss | Draw | Unfinished | Error
  deriving (Eq, Show)

instance Show Move where
  show Nought = "O"
  show Cross  = "X"

instance Show Cell where
  show Cell{ cellState=(Just move) }  = show move
  show _                              = "E"

parseB :: String -> Board
parseB s = 
  (uncurry Cell) <$> zip [0..] (map parseB' s)
    where 
      parseB' 'O' = Just Nought
      parseB' 'X' = Just Cross
      parseB' _   = Nothing

moveRowCol :: Board -> Move -> Row -> Col -> Board
moveRowCol b m r c 
  | legalMove b m cellPos = moveCellPos b m cellPos
  | otherwise             = undefined
  where
    cellPos = rowColToCellPos r c boardWidth

legalMove :: Board -> Move -> CellPos -> Bool
legalMove b m pos = 
  boardIsUnfinished && isPlayersTurn && cellIsVacant && isInsideBoard
    where
      boardIsUnfinished   = state b m == Unfinished
      isPlayersTurn       = turn b == m
      cellIsVacant        = cellStateAt b pos == Nothing
      isInsideBoard       = pos <= numberOfCells

state :: Board -> Player -> Result
state b p 
  | rowStrike == Just p             = Win
  | rowStrike == Just (opponent p)  = Loss 
  | otherwise                       = Error
    where 
      rowStrike = checkForStrikeH b
      boardIsFull = all (isJust . cellState) b
      boardNotFull = not boardIsFull

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

turn :: Board -> Move
turn = undefined

cellStateAt :: Board -> CellPos -> CellState
cellStateAt b pos = undefined

rowColToCellPos :: Row -> Col -> Int -> CellPos
rowColToCellPos r c w = ( r * w ) + c

moveCellPos :: Board -> Move -> CellPos -> Board
moveCellPos b m pos = undefined

result :: Board -> Move -> CellPos -> Result
result = undefined

opponent :: Player -> Player
opponent Nought = Cross
opponent Cross  = Nought

boardToRows :: Board -> [[Cell]]
boardToRows = chunksOf boardWidth

emptyBoard :: Board
emptyBoard = flip Cell Nothing <$> [0..8]
