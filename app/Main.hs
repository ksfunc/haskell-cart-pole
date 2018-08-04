module Main where

import System.Environment 
import Learner

main :: IO ()
main = do
  args <- getArgs
  let nEpisodes = read $ args !! 0
      nSteps = read $ args !! 1
      nStates = read $ args !! 2
  successful <- learn nEpisodes nSteps nStates
  putStr "Is the game successful?: "
  print successful
