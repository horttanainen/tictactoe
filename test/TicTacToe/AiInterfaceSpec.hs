module TicTacToe.AiInterfaceSpec where

import TicTacToe.AiInterface

import Test.Hspec

spec =
  describe "newEnvironment" $
    it "should return an empty state" $
      True `shouldBe` False
