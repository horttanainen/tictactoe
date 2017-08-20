module TicTacToe.HumanInterface.Internals
  ( module TicTacToe.HumanInterface.Internals
  , module TicTacToe.Domain
  ) where

import TicTacToe.Domain

rowColToCellPos :: Row -> Col -> CellPos
rowColToCellPos r c = ( r * 3 ) + c

