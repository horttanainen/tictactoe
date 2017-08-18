module CoreSpec where

import Core
import Test.Hspec

rowOfNoughts :: [Cell]
rowOfNoughts = ((flip Cell) (Just Nought)) <$> [0..2]

rowOfCrosses :: [Cell]
rowOfCrosses = ((flip Cell) (Just Cross)) <$> [0..2]

emptyRow  :: [Cell]
emptyRow = ((flip Cell) Nothing) <$> [0..2]

oneNoughtTwoCrosses :: [Cell]
oneNoughtTwoCrosses = head rowOfNoughts : (init rowOfCrosses)

oneCrossTwoNoughts :: [Cell]
oneCrossTwoNoughts = head rowOfCrosses : (init rowOfNoughts)

twoCrossesOneNought :: [Cell]
twoCrossesOneNought = take 2 rowOfCrosses ++ [(head rowOfNoughts)]

twoNoughtsOneCross :: [Cell]
twoNoughtsOneCross = take 2 rowOfNoughts ++ [(head rowOfCrosses)]

noughtWinH :: Board
noughtWinH = (((flip Cell) (Just Nought)) <$> [0..2]) 
  ++ ((flip Cell Nothing) <$> [3..8])

crossWinH :: Board
crossWinH = (((flip Cell) (Just Cross)) <$> [0..2]) 
  ++ ((flip Cell Nothing) <$> [3..8])

spec = do
  describe "parseB" $ do
    it "parses a board from given string" $ do
      parseB "OOOEEEEEE" `shouldBe` noughtWinH
      parseB "XXXEEEEEE" `shouldBe` crossWinH
      parseB "OOX" `shouldBe` twoNoughtsOneCross
      parseB "XXO" `shouldBe` twoCrossesOneNought
      parseB "XOO" `shouldBe` oneCrossTwoNoughts

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
      checkForStrikeRow rowOfNoughts `shouldBe` Just Nought

    it "should return Just Cross for list of Just Crosses" $ do
      checkForStrikeRow rowOfCrosses `shouldBe` Just Cross
      
    it "should return Nothing for everything else" $ do
      checkForStrikeRow rowOfCrosses `shouldBe` Just Cross
      checkForStrikeRow twoCrossesOneNought `shouldBe` Nothing
      checkForStrikeRow twoNoughtsOneCross `shouldBe` Nothing
      checkForStrikeRow oneCrossTwoNoughts `shouldBe` Nothing
      checkForStrikeRow oneNoughtTwoCrosses `shouldBe` Nothing

  describe "checkForStrikeH" $ do
    it "should return the player who has a horizontal strike or empty" $ do
      checkForStrikeH noughtWinH `shouldBe` Just Nought
      checkForStrikeH crossWinH  `shouldBe` Just Cross
      checkForStrikeH emptyBoard `shouldBe` Nothing

  describe "state" $ do
    it "should returns state of board from players pov" $ do
      state noughtWinH Nought `shouldBe` Win
      state crossWinH Nought `shouldBe` Loss
      state noughtWinH Cross `shouldBe` Loss
      state crossWinH Cross `shouldBe` Win

