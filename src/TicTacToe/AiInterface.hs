{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TicTacToe.AiInterface where

import Data.Maybe
import Control.Monad.State hiding (state)
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

type Input  = Vector R
type W1     = Matrix R
type W2     = Matrix R
type W3     = Matrix R
type Output = Vector R
type B      = Matrix R

type Hidden1  = Matrix R
type Hidden2  = Matrix R

type Reward = R

type ActionIndex = Int

type Activator = Vector R -> Vector R
type LossFunction = Vector R -> Vector R -> Vector R

data Model = Model {
  w1        :: W1,
  w2        :: W2,
  w3        :: W3,
  b1        :: B,
  b2        :: B,
  b3        :: B
}

newtype Target = Target {
  unTarget :: Model
}

data NeuralNetwork = NN {
  epsilon       :: Double,
  model         :: Model,
  targetModel   :: Target,
  input         :: Input,
  buffer        :: [BufferData],
  adamS         :: AdamState,
  randomGen     :: StdGen
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
  adamP             :: AdamParameters
}

data BufferData = BufferD {
  state     :: Input,
  action    :: ActionIndex,
  reward    :: Reward,
  nextState :: Input,
  done      :: Bool
}

type Network' a = ReaderT HyperParameters (State NeuralNetwork) a

newtype Network a = Network {
  unNetwork :: Network' a
} deriving (Monad, Applicative, Functor, MonadReader HyperParameters,
  MonadState NeuralNetwork)

replay :: Network ()
replay = do
  epsMin  <- asks epsilonMin
  epsD    <- asks epsilonDecay 
  bSize   <- asks batchSize 
  buffer  <- gets buffer
  sample  <- randomSample bSize buffer
  forM_ sample (\bufferD -> do
    target <- calcTarget bufferD
    fitModel (state bufferD) target
    s@NN{epsilon = eps} <- get
    when (eps > epsMin) $ put s{epsilon = eps * epsD})
        
calcTarget :: BufferData -> Network Output
calcTarget (BufferD state action reward nextState done) = do
  model <- gets model
  targetModel <- gets targetModel
  gamma <- asks gamma
  predCurRew <- predict model state
  if done 
    then
      return $ fromList $ replace (toList predCurRew) action reward
    else do
      predFutRew <- predict model nextState
      targetPredFutRew <- predict (unTarget targetModel) nextState
      return $ fromList $ replace (toList predCurRew) action $ reward + gamma * targetPredFutRew ! maxIndex predFutRew
    where
      replace xs index value =
        let (before, _:after) = splitAt (action - 1) xs
        in before ++ (value:after)

fitModel :: Input -> Output -> Network ()
fitModel state target = do
  m@(Model w1 w2 w3 b1 b2 b3) <- gets model
  let state' = asColumn state -- [9x1]
      target' = asColumn target -- [9x1]
      hidden1 = relu $ b1 + (w1 <> state') -- [24x9] x [9x1] = [24x1]
      hidden2 = relu $ add b2 $ w2 <> hidden1 -- [24x24] x [24x1] = [24x1]
      prediction = add b3 $ w3 <> hidden2 -- [9x24] x [24x1] = [9x1]
      loss = meanSquaredError target' prediction -- [9x1]
      g1 = w1 <> state' + b1 -- [24x9] x [9x1] = [24x1]
      g2 = w2 <> hidden1 + b2 -- [24x24] x [24x1] = [24x1]
      dg3 = - (target' - prediction) -- [9x1]
      dg2 = drelu g1 * (tr' dg3 <> w3) -- [1x9] x [9x24] = [1x24]
      dg1 = drelu g2 * dg2 <> w2 -- [1x24] x [24x24] = [1x24]
      dw3 = dg3 <> tr' hidden2 -- [9x1] x [1x24] = [9x24]
      db3 = dg3 -- [9x1]
      dw2 = tr' dg2 <> tr' hidden1 -- [24x1] x [1x24] = [24x24]
      db2 = tr' dg2 -- [24x1]
      dw1 = tr' dg1 <> tr' state' -- [24x1] x [1x9] = [24x9]
      db1 = tr' dg1 -- [24x1]
      drelu g = fromLists $ map (map cutOff) $ toLists g 
      cutOff el = if el <= 0 then 0 else 1
      
  return ()

meanSquaredError :: Floating a => a -> a -> a
meanSquaredError target prediction = 
  0.5 * ((target - prediction)**2)

act :: Network ActionIndex
act = do
  eps <- gets epsilon
  (rNumber :: Double) <- randomNumber (0,1)
  if rNumber <= eps
    then randomAction
    else do
      model <- gets model
      input <- gets input
      maxIndex <$> predict model input

randomAction :: Network ActionIndex
randomAction = do
  actions <- legalActions
  randomElement actions
    where
      legalActions :: Network [ActionIndex]
      legalActions = do
        input <- gets input
        return $ find (==0) input

predict :: Model -> Input -> Network Output
predict (Model w1 w2 w3 b1 b2 b3) input = do
  let input' = asColumn input -- [9x1]
      hidden1 = relu $ add b1 $ w1 <> input' -- [24x9] x [9x1] = [24x1]
      hidden2 = relu $ add b2 $ w2 <> hidden1 -- [24x24] x [24x1] = [24x1]
  return (flatten $ add b3 $ w3 <> hidden2) -- [9x24] x [24x1] = [9x1]

remember :: BufferData -> Network ()
remember bData = do
  s@NN{ buffer=buffer }   <- get
  bSize                   <- asks bufferSize
  if length buffer < bSize
    then put s{ buffer = bData : buffer } 
    else put s{ buffer = bData : init buffer }

adam :: Network ()
adam = assert False undefined

relu :: Matrix R -> Matrix R
relu m =
  fromLists $ map (map relu) rows
    where
      rows = toLists m
      relu = max 0

initHyp :: HyperParameters
initHyp = assert False undefined

initNN :: Reader HyperParameters NeuralNetwork
initNN = assert False undefined

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

