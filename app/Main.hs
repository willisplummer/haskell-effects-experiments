module Main where

import Control.Monad.Freer (run)
import Control.Monad.Freer.State (evalState)
-- import Control.Monad.Logger (runStdoutLoggingT)
import Lib

main :: IO ()
main = do
  _ <- return . run $ evalState (GameState 12) playGame
  return ()
