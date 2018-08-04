module Learner (learn) where

import System.Random
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Data.Maybe
import qualified Data.Map as Map

data Environment = Environment { ePosition :: Double
                               , eVelocity :: Double
                               , eTheta :: Double
                               , eOmega :: Double
                               } deriving (Show)

data EState = EState { esPosition :: Int
                     , esVelocity :: Int
                     , esTheta :: Int
                     , esOmega :: Int
                     } deriving (Show, Eq, Ord)

data Action = PushCartToTheRight
            | PushCartToTheLeft deriving (Show, Eq, Ord)

type QTable = Map.Map EState (Map.Map Action Double)

data Context = Context { cEnvironment :: Environment
                       , cEState :: EState
                       , cQTable :: QTable
                       } deriving (Show)

data Constants = Constants { cNStates :: Int }

learn :: Int -> Int -> Int -> IO Bool
learn nEpisodes nSteps nStates = do
  let consts = Constants { cNStates = nStates }
  failed <- (`runReaderT` consts) $ do
    ctx <- initialContext
    (`evalStateT` ctx) $ learnEpisodes nEpisodes nSteps
  return $ not failed

learnEpisodes :: Int -> Int -> StateT Context (ReaderT Constants IO) Bool
learnEpisodes 1 nSteps = learnSteps 0 nSteps
learnEpisodes nEpisodes nSteps = do
  learnEpisodes (nEpisodes - 1) nSteps
  setNextContext
  learnSteps (nEpisodes - 1) nSteps

setNextContext :: StateT Context (ReaderT Constants IO) ()
setNextContext = do
  env <- liftIO initialEnvironment
  consts <- lift ask
  let n = cNStates consts
      es = toEState env n
  ctx <- get
  put ctx { cEnvironment = env
          , cEState = es }

learnSteps :: Int -> Int -> StateT Context (ReaderT Constants IO) Bool
learnSteps episode 1 = learnStep episode
learnSteps episode nSteps = do
  failed <- learnStep episode
  if failed
    then return True
    else learnSteps episode (nSteps - 1)

learnStep :: Int -> StateT Context (ReaderT Constants IO) Bool
learnStep episode = do
  action <- chooseAction episode
  (failed, reward, nextEState) <- takeAction action
  updateQTable action reward nextEState
  setNextEState nextEState
  return failed

initialContext :: ReaderT Constants IO Context
initialContext = do
  env <- lift initialEnvironment
  consts <- ask
  let n = cNStates consts
      es = toEState env n
  qt <- initialQTable
  return Context { cEnvironment = env
                 , cEState = es
                 , cQTable = qt }

initialEnvironment :: IO Environment
initialEnvironment = do
  rPosition <- randomRIO (-1.2, 1.2) :: IO Double
  rVelocity <- randomRIO (-1.5, 1.5) :: IO Double
  rTheta <- randomRIO (-0.25, 0.25) :: IO Double
  rOmega <- randomRIO (-1.0, 1.0) :: IO Double  
  return Environment { ePosition = rPosition
                     , eVelocity = rVelocity
                     , eTheta = rTheta
                     , eOmega = rOmega }

initialQTable :: ReaderT Constants IO QTable
initialQTable = do
  qs <- forM actions $ \action -> do
    rand <- lift (randomIO :: IO Double)
    return (action, rand)
  ess <- eStates
  let aToQ = Map.fromList $ qs
      ms = map (\es -> (es, aToQ)) ess
  return $ Map.fromList ms

actions :: [Action]
actions = [PushCartToTheRight, PushCartToTheLeft]

eStates :: ReaderT Constants IO [EState]
eStates = do
  consts <- ask
  let n = cNStates consts
  return $ do
    v0 <- [0 .. n - 1]
    v1 <- [0 .. n - 1]
    v2 <- [0 .. n - 1]
    v3 <- [0 .. n - 1]
    return EState { esPosition = v0
                  , esVelocity = v1
                  , esTheta = v2
                  , esOmega = v3 }

chooseAction :: Int -> StateT Context (ReaderT Constants IO) Action
chooseAction episode = do
  let epsilon = 0.5 * (1.0 / (fromIntegral episode + 1.0))
  rand <- liftIO (randomIO :: IO Double)
  if epsilon <= rand
    then greedyAction
    else liftIO $ randomChoice actions

argMax :: Ord a => Map.Map k a -> k
argMax m = fst $ foldr1 f $ Map.toList m
  where f (k, x) (ak, ax) = if x >= ax then (k, x) else (ak, ax)

greedyAction :: StateT Context (ReaderT Constants IO) Action
greedyAction = do
  ctx <- get
  let es = cEState ctx
      qt = cQTable ctx
      aToQ = fromJust $ Map.lookup es qt
  return $ argMax aToQ

randomChoice :: [a] -> IO a
randomChoice xs = do
  rand <- randomRIO (0, length xs - 1) :: IO Int
  return $ xs !! rand

takeAction :: Action ->
              StateT Context (ReaderT Constants IO) (Bool, Double, EState)
takeAction action = do
  ctx <- get
  consts <- lift ask
  let env = cEnvironment ctx
      nextEnv = calculate env action
      n = cNStates consts
      failed = isFailed nextEnv
      reward = if failed then -200.0 else 1.0
      nextEState = toEState nextEnv n
  put ctx { cEnvironment = nextEnv }
  return (failed, reward, nextEState)

-- FIXME
calculate :: Environment -> Action -> Environment
calculate env action = env { ePosition = nPosition
                           , eVelocity = nVelocity
                           , eTheta = nTheta
                           , eOmega = nOmega }
  where nPosition = ePosition env + eVelocity env / 100.0
        nVelocity = case action of
                      PushCartToTheRight -> eVelocity env + 0.3
                      PushCartToTheLeft -> eVelocity env - 0.3
        nTheta = eTheta env + eOmega env / 100.0
        nOmega = case action of
                   PushCartToTheRight -> eOmega env - 0.2
                   PushCartToTheLeft -> eOmega env + 0.2

isFailed :: Environment -> Bool
isFailed env = dPosition || dTheta
  where dPosition = ePosition env < -2.4 || 2.4 < ePosition env
        dTheta = eTheta env < -0.5 || 0.5 < eTheta env

digitize :: Double -> Double -> Int -> Double -> Int
digitize from to n m = length $ takeWhile (<= m) us
  where d = (to - from) / fromIntegral n
        us = map (\x -> fromIntegral x * d + from) [1 .. n - 1]

toEState :: Environment -> Int -> EState
toEState env n = EState { esPosition = digitize (-2.4) 2.4 n (ePosition env)
                        , esVelocity = digitize (-3.0) 3.0 n (eVelocity env)
                        , esTheta = digitize (-0.5) 0.5 n (eTheta env)
                        , esOmega = digitize (-2.0) 2.0 n (eOmega env) }
        
updateQTable :: Action -> Double -> EState ->
                StateT Context (ReaderT Constants IO) ()
updateQTable action reward nextEState = do
  ctx <- get
  let es = cEState ctx
      qt = cQTable ctx
      aToQ = fromJust $ Map.lookup nextEState qt
      nextMaxQ = maximum $ Map.elems aToQ
      update q = q + alpha * (reward + gamma * nextMaxQ - q)
      nextQTable = Map.adjust (Map.adjust update action) es qt
  put ctx { cQTable = nextQTable }
  where alpha = 0.5
        gamma = 0.99

setNextEState :: EState -> StateT Context (ReaderT Constants IO) ()
setNextEState nextEState = do
  ctx <- get
  put ctx { cEState = nextEState }
