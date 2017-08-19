module TicTacToe.Domain where

data Move = Nought | Cross deriving (Eq, Ord)

type Player = Move

type CellState = Maybe Move

data Cell = Cell {
  cellPos :: CellPos,
  cellState :: CellState
} deriving (Eq, Ord)

type Board = [Cell]

type CellPos = Int
type Row = Int
type Col = Int

type Run = [Board]

data Result = Win | Loss | Draw | Unfinished | Error
  deriving (Eq, Show)

instance Show Move where
  show Nought = "O"
  show Cross  = "X"

instance Show Cell where
  show Cell{ cellState=(Just move) }  = show move
  show _                              = "E"

