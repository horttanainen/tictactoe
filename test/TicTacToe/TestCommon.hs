module TicTacToe.TestCommon where

import TicTacToe.Domain

parseB :: String -> Board
parseB s = 
  uncurry Cell <$> zip [0..] (map parseB' s)
    where 
      parseB' 'O' = Just Nought
      parseB' 'X' = Just Cross
      parseB' _   = Nothing

