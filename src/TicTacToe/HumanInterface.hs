module TicTacToe.HumanInterface
  ( module TicTacToe.HumanInterface
  , module TicTacToe.Domain
  ) where

import TicTacToe.HumanInterface.Internals
import TicTacToe.Domain
import TicTacToe.Core as C

move :: Board -> Move -> Row -> Col -> Maybe Board
move b m r c =
  C.move b m cellPos
    where cellPos = rowColToCellPos r c

emptyBoard :: Board
emptyBoard = flip Cell Nothing <$> [0..8]

