{-# LANGUAGE MonomorphismRestriction #-}

module TicTacToe.AiInterface where

import Data.Maybe
import Control.Monad.State
import Control.Monad.Reader
import System.Random (StdGen, split, randomR, Random)
import System.Random.Shuffle (shuffle')

import Numeric.LinearAlgebra.Data

import TicTacToe.Core (move, result)
import TicTacToe.Domain (Result(..), Board, Move, CellPos)
import TicTacToe.HumanInterface.Internals (rowColToCellPos)

class Player a where
  predictedAction :: BoardMatrix -> a -> Action

data GameState = GameState {
  reward' :: Int,
  done' :: Bool,
  board :: BoardMatrix
}

type BoardMatrix = Matrix Z
type Action = (Int, Int)

step :: (Player a) => BoardMatrix -> Action -> a -> GameState
step m action opponent =
  case aiResult m action of
    Error   -> err
    Win     -> win
    Draw    -> draw
    _       -> opponentMoveToState (fromJust (aiMove m action)) opponent

opponentMoveToState :: (Player a) => BoardMatrix -> a -> GameState
opponentMoveToState m opponent =
  let
    action = predictedAction m opponent
  in
    case aiResult m action of
      Win         -> loss
      Draw        -> draw
      Unfinished  -> unfinished m action

aiMove :: BoardMatrix -> Action -> Maybe BoardMatrix
aiMove matrix action =
  let 
    board           = matrixToBoard matrix
    (pl, cellPos)   = actionToPlayerCellPos action
  in
    case move board pl cellPos of
      Just b  -> Just $ boardToMatrix b
      _       -> Nothing

aiResult :: BoardMatrix -> Action -> Result
aiResult matrix action =
  result board pl cellPos
  where 
    board           = matrixToBoard matrix
    (pl, cellPos)   = actionToPlayerCellPos action

actionToPlayerCellPos :: Action -> (Move, CellPos)
actionToPlayerCellPos = undefined

matrixToBoard :: BoardMatrix -> Board
matrixToBoard = undefined

boardToMatrix :: Board -> BoardMatrix
boardToMatrix = undefined

matrixToState :: BoardMatrix -> GameState
matrixToState = undefined

emptyGameState :: GameState
emptyGameState = undefined

err :: GameState
err = undefined

loss :: GameState
loss = undefined

win :: GameState
win = undefined

draw :: GameState
draw = undefined

unfinished :: BoardMatrix -> Action -> GameState
unfinished m a = undefined

-- NN stuff

type Input  = Matrix Z
type W1     = Matrix R
type W2     = Matrix R
type Output = Matrix R

type Reward = Int

data NeuralNetwork = NN {
  epsilon :: Double,
  input     :: Input,
  w1        :: W1,
  w2        :: W2,
  output    :: Output,
  buffer    :: [BufferData],
  adamS     :: AdamState,
  randomGen :: StdGen
}

data AdamParameters = AdamP {
  eps       :: Double,
  beta1     :: Double,
  beta2     :: Double
}

data AdamState = Adam {
  iteration :: Int,
  mass      :: Double,
  velocity  :: Double
}

data HyperParameters = Hyp {
  player  :: Move,
  epsilonDecay :: Double,
  epsilonMin :: Double,
  gamma :: Double,
  learningRate :: Double,
  learningRateDecay :: Double,
  episodes :: Int,
  batchSize :: Int,
  bufferSize :: Int,
  stateSize :: Int,
  adamP :: AdamParameters
}

data BufferData = BufferD {
  state     :: Input,
  action    :: Action,
  reward    :: Reward,
  nextState :: Input,
  done      :: Bool
}

type StatePlusParams = ReaderT HyperParameters (State NeuralNetwork)

replay ::  StatePlusParams ()
replay = do
  sample <- randomSample
  forM_ sample (\(BufferD state action reward nextState done) -> do
      trgt <- target reward state nextState
      fit state trgt
      Hyp{epsilonMin = epsMin, epsilonDecay = epsD} <- ask
      s@NN{epsilon = eps} <- get
      when (eps > epsMin) $ put s{epsilon = eps * epsD})

randomSample :: StatePlusParams [BufferData]
randomSample = do
  s@NN{ buffer=buffer, randomGen=gen }  <- get
  Hyp{ batchSize=bSize }                <- ask
  randomSample' bSize buffer

randomSample' :: Int -> [a] -> StatePlusParams [a]
randomSample' n xs =
  take n <$> randomShuffle' xs

randomShuffle' :: [a] -> StatePlusParams [a]
randomShuffle' xs = do
  s@NN{ randomGen=gen }  <- get
  let shuffled  = shuffle' xs (length xs) gen
      (gen', _) = split gen
  put s{ randomGen=gen' }
  return shuffled

randomElement :: [a] -> StatePlusParams a
randomElement xs =
  head <$> randomShuffle' xs
    
act :: StatePlusParams Action
act = do
  s@NN{ epsilon=eps }  <- get
  rNumber <- randomNumber (0,1) :: StatePlusParams Double
  if rNumber <= eps
    then randomAction
    else maxIndex <$> predict

randomNumber :: (Random a) => (a, a) -> StatePlusParams a
randomNumber (beg, end) = do
  s@NN{ randomGen=gen }  <- get
  let (rNumber, gen') = randomR (beg, end) gen
  put s{randomGen=gen'}
  return rNumber

randomAction :: StatePlusParams Action
randomAction = do
  choices <- legalActions
  head <$> randomSample' 1 choices

legalActions :: StatePlusParams [Action]
legalActions = do
  s@NN{ input=input }  <- get
  return $ find (==0) input

predict :: StatePlusParams Output
predict = undefined

remember :: BufferData -> State NeuralNetwork ()
remember = undefined

initHyp :: HyperParameters
initHyp = undefined

initNN :: Reader HyperParameters NeuralNetwork
initNN = undefined

forwardPropagation :: StatePlusParams ()
forwardPropagation = undefined

activator :: (Num a) => a -> a
activator = undefined

target :: Reward -> Input -> Input -> StatePlusParams Output
target = undefined

fit :: Input -> output -> StatePlusParams ()
fit = undefined

adam :: StatePlusParams ()
adam = undefined

