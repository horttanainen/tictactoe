module CoreSpec where

import Core
import Test.Hspec

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
      state (parseB "OOOXXEXEE") Nought `shouldBe` Win
      state (parseB "XXXOOEOEE") Nought `shouldBe` Loss
      state (parseB "OOOXXEXEE") Cross `shouldBe` Loss
      state (parseB "XXXOOEOEE") Cross `shouldBe` Win

