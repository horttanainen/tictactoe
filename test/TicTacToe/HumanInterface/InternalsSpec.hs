module TicTacToe.HumanInterface.InternalsSpec where

import Test.Hspec

import TicTacToe.HumanInterface.Internals

spec = do
  describe "rowColToCellPos" $ do
    it "should convert row and col to cell position" $ do
      rowColToCellPos 0 0 `shouldBe` 0
      rowColToCellPos 2 2 `shouldBe` 8
      rowColToCellPos 1 2 `shouldBe` 5

