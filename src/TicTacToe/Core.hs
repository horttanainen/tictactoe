module TicTacToe.Core
  ( module TicTacToe.Core
  , module TicTacToe.Domain
  ) where

import Data.Maybe

import TicTacToe.Core.Internals
import TicTacToe.Domain

move :: Board -> Move -> CellPos -> Board
move b m pos 
  | legal b m pos = moveNoCheck b m pos
  | otherwise         = undefined

state :: Board -> Player -> Result
state b p 
  | rowStrike == Just p               = Win
  | rowStrike == Just (opponent p)    = Loss 
  | columnStrike == Just p            = Win
  | columnStrike == Just (opponent p) = Loss
  | diagStrike == Just p              = Win
  | diagStrike == Just (opponent p)   = Loss
  | boardIsFull                       = Draw
  | otherwise                         = Unfinished
    where 
      rowStrike = checkForStrikeH b
      columnStrike = checkForStrikeV b
      diagStrike = checkForStrikeD b
      boardIsFull = all (isJust . cellState) b

result :: Board -> Move -> CellPos -> Result
result b m pos
  | legal b m pos = state (moveNoCheck b m pos) m
  | otherwise     = Error

opponent :: Player -> Player
opponent Nought = Cross
opponent Cross  = Nought

