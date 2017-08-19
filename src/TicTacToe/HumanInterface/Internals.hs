module TicTacToe.HumanInterface.Internals
  ( module TicTacToe.HumanInterface.Internals
  , module TicTacToe.Domain
  ) where

import TicTacToe.Core (CellPos)
import TicTacToe.Domain

rowColToCellPos :: Row -> Col -> CellPos
rowColToCellPos r c w = ( r * 3 ) + c

