{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TicTacToe.AiInterface where

import Data.Maybe
import Control.Monad.State
import Control.Monad.Reader
import Control.Exception.Base (assert)
import System.Random (StdGen, split, randomR, Random)
import System.Random.Shuffle (shuffle')

import Numeric.LinearAlgebra

import TicTacToe.Core (move, result)
import TicTacToe.Domain (Result(..), Board, Move, CellPos)

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
  let pAction = predictedAction m opponent
  in
    case aiResult m pAction of
      Win         -> loss
      Draw        -> draw
      Unfinished  -> unfinished m pAction

aiMove :: BoardMatrix -> Action -> Maybe BoardMatrix
aiMove matrix action =
  let board           = matrixToBoard matrix
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
actionToPlayerCellPos = assert False undefined

matrixToBoard :: BoardMatrix -> Board
matrixToBoard = assert False undefined

boardToMatrix :: Board -> BoardMatrix
boardToMatrix = assert False undefined

matrixToState :: BoardMatrix -> GameState
matrixToState = assert False undefined

emptyGameState :: GameState
emptyGameState = assert False undefined

err :: GameState
err = assert False undefined

loss :: GameState
loss = assert False undefined

win :: GameState
win = assert False undefined

draw :: GameState
draw = assert False undefined

unfinished :: BoardMatrix -> Action -> GameState
unfinished m a = assert False undefined

-- NN stuff

type Input  = Matrix R
type W1     = Matrix R
type W2     = Matrix R
type Output = Matrix R
type B1     = Matrix R
type B2     = Matrix R
type B3     = Matrix R

type Hidden1  = Matrix R
type Hidden2  = Matrix R
type Scores   = Matrix R

type Reward = Int

type Activator = Matrix R -> Matrix R

data NeuralNetwork = NN {
  epsilon   :: Double,
  input     :: Input,
  w1        :: W1,
  w2        :: W2,
  output    :: Output,
  b1        :: B1,
  b2        :: B2,
  b3        :: B3,
  hidden1   :: Hidden1,
  hidden2   :: Hidden2,
  scores    :: Scores,
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
  player            :: Move,
  epsilonDecay      :: Double,
  epsilonMin        :: Double,
  gamma             :: Double,
  learningRate      :: Double,
  learningRateDecay :: Double,
  episodes          :: Int,
  batchSize         :: Int,
  bufferSize        :: Int,
  stateSize         :: Int,
  adamP             :: AdamParameters,
  w1Activator       :: Activator,
  w2Activator       :: Activator,
  outActivator      :: Activator
}

data BufferData = BufferD {
  state     :: Input,
  action    :: Action,
  reward    :: Reward,
  nextState :: Input,
  done      :: Bool
}

type Network' a = ReaderT HyperParameters (State NeuralNetwork) a

newtype Network a = Network {
  unNetwork :: Network' a
} deriving (Monad, Applicative, Functor, MonadReader HyperParameters,
  MonadState NeuralNetwork)

replay ::  Network ()
replay = do
  epsMin  <- asks epsilonMin
  epsD    <- asks epsilonDecay 
  bSize   <- asks batchSize 
  buffer  <- gets buffer
  sample  <- randomSample bSize buffer
  forM_ sample (\(BufferD state action reward nextState done) -> do
      trgt <- target reward state nextState
      fit state trgt
      s@NN{epsilon = eps} <- get
      when (eps > epsMin) $ put s{epsilon = eps * epsD})

randomSample :: Int -> [a] -> Network [a]
randomSample n xs =
  take n <$> randomShuffle xs

randomShuffle :: [a] -> Network [a]
randomShuffle xs = do
  s@NN{ randomGen=gen }  <- get
  let shuffled  = shuffle' xs (length xs) gen
      (gen', _) = split gen
  put s{ randomGen=gen' }
  return shuffled

randomElement :: [a] -> Network a
randomElement xs = do
  randomIndex :: Int <- randomNumber (0, length xs)
  return $ xs !! randomIndex
    
randomNumber :: (Random a) => (a, a) -> Network a
randomNumber (beg, end) = do
  s@NN{ randomGen=gen }  <- get
  let (rNumber, gen') = randomR (beg, end) gen
  put s{randomGen=gen'}
  return rNumber

act :: Network Action
act = do
  eps <- gets epsilon
  (rNumber :: Double) <- randomNumber (0,1)
  if rNumber <= eps
    then randomAction
    else maxIndex <$> predict

randomAction :: Network Action
randomAction = do
  actions <- legalActions
  randomElement actions
    where
      legalActions :: Network [Action]
      legalActions = do
        input <- gets input
        return $ find (==0) input

predict :: Network Output
predict = do
  
forwardPropagate :: Network ()
forwardPropagate = do
  input     <- gets input
  w1        <- gets w1
  w2        <- gets w2
  output    <- gets output
  b1        <- gets b1
  b2        <- gets b2
  b3        <- gets b3
  w1Act     <- asks w1Activator
  w2Act     <- asks w2Activator
  outAct    <- asks outActivator
  let hidden1 = w1Act $ add b1 $ w1 <> input
  let hidden2 = w2Act $ add b2 $ w2 <> hidden1
  let scores = (outAct $ add b3 $ output <> hidden2)

linearActivator :: Activator
linearActivator x = x

reluActivator :: Activator
reluActivator matrix =
  fromLists $ map (map relu) rows
    where
      relu = max 0
      rows = toLists matrix

remember :: BufferData -> Network ()
remember bData = do
  s@NN{ buffer=buffer }   <- get
  bSize                   <- asks bufferSize
  if length buffer < bSize
    then put s{ buffer = bData : buffer } 
    else put s{ buffer = bData : init buffer }

initHyp :: HyperParameters
initHyp = assert False undefined

initNN :: Reader HyperParameters NeuralNetwork
initNN = assert False undefined

activator :: (Num a) => a -> a
activator = assert False undefined

target :: Reward -> Input -> Input -> Network Output
target = assert False undefined

fit :: Input -> output -> Network ()
fit = assert False undefined

adam :: Network ()
adam = assert False undefined

