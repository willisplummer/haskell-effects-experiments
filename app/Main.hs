module Main where

import Control.Monad.State (runStateT)
import Control.Monad.Logger (runStdoutLoggingT)
import Lib

main :: IO ()
main = do
  _ <- runStdoutLoggingT $ runStateT playGame (GameState 12)
  return ()
