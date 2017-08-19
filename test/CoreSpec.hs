module CoreSpec where

import Core
import Test.Hspec

parseB :: String -> Board
parseB s = 
  (uncurry Cell) <$> zip [0..] (map parseB' s)
    where 
      parseB' 'O' = Just Nought
      parseB' 'X' = Just Cross
      parseB' _   = Nothing

spec = do
  describe "rowColToCellPos" $ do
    it "should convert row and col to cell position" $ do
      rowColToCellPos 0 0 3 `shouldBe` 0
      rowColToCellPos 2 2 3 `shouldBe` 8
      rowColToCellPos 1 2 3 `shouldBe` 5

  describe "opponent" $ do
    it "should return the opponent for given player" $ do
      opponent Nought `shouldBe` Cross
      opponent Cross `shouldBe` Nought

  describe "checkForStrikeRow" $ do
    it "should return Just Nought for list of Just Noughts" $ do
      checkForStrikeRow (parseB "OOO") `shouldBe` Just Nought

    it "should return Just Cross for list of Just Crosses" $ do
      checkForStrikeRow (parseB "XXX") `shouldBe` Just Cross
      
    it "should return Nothing for everything else" $ do
      checkForStrikeRow (parseB "OXO") `shouldBe` Nothing
      checkForStrikeRow (parseB "OOX") `shouldBe` Nothing
      checkForStrikeRow (parseB "EXX") `shouldBe` Nothing
      checkForStrikeRow (parseB "XXE") `shouldBe` Nothing

  describe "checkForStrikeH" $ do
    it "should return the player who has a horizontal strike or empty" $ do
      checkForStrikeH (parseB "OOOXXEXEE") `shouldBe` Just Nought
      checkForStrikeH (parseB "OOXEEOXXX") `shouldBe` Just Cross
      checkForStrikeH (parseB "EEEEEEEEE") `shouldBe` Nothing
      checkForStrikeH (parseB "OOXXOOXXO") `shouldBe` Nothing

  describe "checkForStrikeV" $ do
    it "should return the player who has a vertical strike or empty" $ do
      checkForStrikeV (parseB "OOOXXEXEE") `shouldBe` Nothing
      checkForStrikeV (parseB "OOXEEOXXX") `shouldBe` Nothing
      checkForStrikeV (parseB "OXOOXEOOX") `shouldBe` Just Nought
      checkForStrikeV (parseB "XEEXOOXOE") `shouldBe` Just Cross

  describe "checkForStrikeD" $ do
    it "should return the player who has a diagonal strike or empty" $ do
      checkForStrikeD (parseB "OXEXOXXEO") `shouldBe` Just Nought
      checkForStrikeD (parseB "EEXOXOXOO") `shouldBe` Just Cross
      checkForStrikeD (parseB "OOXEEOXXX") `shouldBe` Nothing

  describe "state" $ do
    it "should returns state of board from players pov" $ do
      state (parseB "OOOXXEXEE") Cross `shouldBe` Loss
      state (parseB "OOXEEOXXX") Cross `shouldBe` Win
      state (parseB "EEEEEEEEE") Cross `shouldBe` Unfinished
      state (parseB "OOXXOOXXO") Cross `shouldBe` Loss
      state (parseB "OXOOXEOOX") Cross `shouldBe` Loss
      state (parseB "XEEXOOXOE") Cross `shouldBe` Win
      state (parseB "OXEXOXXEO") Cross `shouldBe` Loss
      state (parseB "EEXOXOXOO") Cross `shouldBe` Win
      state (parseB "OXOXOXXOX") Cross `shouldBe` Draw
      state (parseB "OOOXXEXEE") Nought `shouldBe` Win
      state (parseB "OOXEEOXXX") Nought `shouldBe` Loss
      state (parseB "EEEEEEEEE") Nought `shouldBe` Unfinished
      state (parseB "OOXXOOXXO") Nought `shouldBe` Win
      state (parseB "OXOOXEOOX") Nought `shouldBe` Win
      state (parseB "XEEXOOXOE") Nought `shouldBe` Loss
      state (parseB "OXEXOXXEO") Nought `shouldBe` Win
      state (parseB "EEXOXOXOO") Nought `shouldBe` Loss
      state (parseB "OXOXOXXOX") Nought `shouldBe` Draw

  describe "result" $ do
    it "should return error for the result of illegal moves" $ do
      result (parseB "XEEEEEEEE") Cross 4 `shouldBe` Error
      result (parseB "EEEEEEEEE") Nought 4 `shouldBe` Error
      result (parseB "XXXOOEEEE") Nought 8 `shouldBe` Error
      result (parseB "XEEEEEEEE") Nought 0 `shouldBe` Error
      result (parseB "EEEEEEEEE") Cross 9 `shouldBe` Error
      result (parseB "EEEEEEEEE") Cross (-1) `shouldBe` Error

    it "should return the result og board after the move from players pov" $ do
      result (parseB "EOOXXEXEE") Nought 0 `shouldBe` Win
      result (parseB "OOXEEOXEX") Cross 7 `shouldBe` Win
      result (parseB "EEEEEEEEE") Cross 6 `shouldBe` Unfinished
      result (parseB "XOXEEEEEE") Nought 4 `shouldBe` Unfinished

  describe "legal" $ do
    it "should return false if attempted move is not legal" $ do
      legal (parseB "XEEEEEEEE") Cross 4 `shouldBe` False
      legal (parseB "EEEEEEEEE") Nought 4 `shouldBe` False
      legal (parseB "XXXOOEEEE") Nought 8 `shouldBe` False
      legal (parseB "XEEEEEEEE") Nought 0 `shouldBe` False
      legal (parseB "EEEEEEEEE") Cross 9 `shouldBe` False
      legal (parseB "EEEEEEEEE") Cross (-1) `shouldBe` False

    it "should return true if attempted move is legal" $ do
      legal (parseB "EEEEEEEEE") Cross 0 `shouldBe` True
      legal (parseB "XOXXEXOOE") Nought 4 `shouldBe` True
