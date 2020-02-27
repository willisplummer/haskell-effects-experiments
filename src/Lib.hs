{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Lib where

import Control.Monad.State
import Control.Monad.Logger ( MonadLogger )
import Control.Monad.Logger.CallStack ( logDebug )

data GameState = GameState { getCount :: Int }

playGame :: (MonadState GameState m, MonadLogger m) => m ()
playGame = do
  s <- get

  put $ GameState $ getCount s + 1

  logDebug "WARNING: Hello World!"
  return ()


