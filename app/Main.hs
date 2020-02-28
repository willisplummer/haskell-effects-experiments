{-# LANGUAGE PolyKinds #-}

module Main where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (runStateT, MonadState)
import Control.Monad.Logger (runStdoutLoggingT)
import Lib
import Yael.Eff
import Yael.Eff.Log

data Unit m = Unit

main :: IO ()
main = do
  _ <- runStdoutLoggingT $ runStateT (runEffT playGame (stdoutLog :<> stateWithLoggingThing)) (GameState 12)
  return ()
  where
    stateWithLoggingThing :: (MonadIO m, MonadState s m, Show s) => (State' s m)
    stateWithLoggingThing = stateWithLogging mtlState stdoutLog
