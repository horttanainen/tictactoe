module TicTacToe.Core.InternalsSpec where

import Test.Hspec

import TicTacToe.Core.Internals
import TicTacToe.TestCommon

spec = do
  describe "checkForStrikeRow" $ do
    it "should return Just Nought for list of Just Noughts" $
      checkForStrikeRow (parseB "OOO") `shouldBe` Just Nought

    it "should return Just Cross for list of Just Crosses" $
      checkForStrikeRow (parseB "XXX") `shouldBe` Just Cross
      
    it "should return Nothing for everything else" $ do
      checkForStrikeRow (parseB "OXO") `shouldBe` Nothing
      checkForStrikeRow (parseB "OOX") `shouldBe` Nothing
      checkForStrikeRow (parseB "EXX") `shouldBe` Nothing
      checkForStrikeRow (parseB "XXE") `shouldBe` Nothing

  describe "checkForStrikeH" $
    it "should return the player who has a horizontal strike or empty" $ do
      checkForStrikeH (parseB "OOOXXEXEE") `shouldBe` Just Nought
      checkForStrikeH (parseB "OOXEEOXXX") `shouldBe` Just Cross
      checkForStrikeH (parseB "EEEEEEEEE") `shouldBe` Nothing
      checkForStrikeH (parseB "OOXXOOXXO") `shouldBe` Nothing

  describe "checkForStrikeV" $
    it "should return the player who has a vertical strike or empty" $ do
      checkForStrikeV (parseB "OOOXXEXEE") `shouldBe` Nothing
      checkForStrikeV (parseB "OOXEEOXXX") `shouldBe` Nothing
      checkForStrikeV (parseB "OXOOXEOOX") `shouldBe` Just Nought
      checkForStrikeV (parseB "XEEXOOXOE") `shouldBe` Just Cross

  describe "checkForStrikeD" $
    it "should return the player who has a diagonal strike or empty" $ do
      checkForStrikeD (parseB "OXEXOXXEO") `shouldBe` Just Nought
      checkForStrikeD (parseB "EEXOXOXOO") `shouldBe` Just Cross
      checkForStrikeD (parseB "OOXEEOXXX") `shouldBe` Nothing

