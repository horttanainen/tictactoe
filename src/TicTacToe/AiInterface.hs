module TicTacToe.AiInterface where

import Data.Maybe
import Control.Monad.State
import Control.Monad.Reader
import System.Random (StdGen)

import Numeric.LinearAlgebra.Data (Matrix)

import TicTacToe.Core (move, result)
import TicTacToe.Domain (Result(..), Board, Move, CellPos)

class Player a where
  predictedAction :: BoardMatrix -> a -> Action

data GameState = GameState {
  reward' :: Int,
  done' :: Bool,
  board :: BoardMatrix
}

type BoardMatrix = Matrix Int
type Action = Matrix Int

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

type Input  = Matrix Int
type W1     = Matrix Double
type W2     = Matrix Double
type Output = Matrix Double

type Reward = Int

data NeuralNetwork = NN {
  input     :: Input,
  w1        :: W1,
  w2        :: W2,
  output    :: Output,
  batch     :: [BatchData],
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
  epsilon :: Double,
  epsilonDecay :: Double,
  epsilonMin :: Double,
  gamma :: Double,
  learningRate :: Double,
  learningRateDecay :: Double,
  episodes :: Int,
  batchSize :: Int,
  bufferSize :: Int,
  actionSize :: Int,
  stateSize :: Int,
  adamP :: AdamParameters
}

data BatchData = BatchD {
  state     :: Input,
  action    :: Action,
  reward    :: Reward,
  nextState :: Input,
  done      :: Bool
}

replay ::  ReaderT HyperParameters (State NeuralNetwork) ()
replay = do
  sample <- randomSample
  mapM_ replay' sample
    where
      replay' (BatchD state action reward nextState done) = do
        trgt <- target reward state nextState
        fit state trgt
        s@Hyp{epsilon = eps, epsilonMin = epsMin, epsilonDecay = epsD} <- ask
        lift $ put s{epsilon = eps * epsD}
        --when (eps > epsMin) $ put s{epsilon = eps * epsD}

randomSample :: ReaderT HyperParameters (State NeuralNetwork) [BatchData]
randomSample = undefined

remember :: BatchData -> State NeuralNetwork ()
remember = undefined

randomAction :: ReaderT HyperParameters (State NeuralNetwork) Action
randomAction = undefined

initHyp :: HyperParameters
initHyp = undefined

initNN :: Reader HyperParameters NeuralNetwork
initNN = undefined

forwardPropagation :: ReaderT HyperParameters (State NeuralNetwork) ()
forwardPropagation = undefined

activator :: (Num a) => a -> a
activator = undefined

target :: Reward -> Input -> Input -> ReaderT HyperParameters (State NeuralNetwork) Output

target = undefined

predict :: State NeuralNetwork Output
predict = undefined

fit :: Input -> output -> ReaderT HyperParameters (State NeuralNetwork) ()
fit = undefined

adam :: ReaderT HyperParameters (State NeuralNetwork) ()
adam = undefined

act :: BoardMatrix -> ReaderT HyperParameters (State NeuralNetwork) Action
act = undefined
