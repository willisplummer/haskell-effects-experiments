{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Lib where

import Control.Monad.Freer (Eff, Member, run, runM)
import Control.Monad.Freer.State (State, get, put)

data GameState = GameState { getCount :: Int }

playGame :: (Member (State GameState) r) => Eff r ()
playGame = do
  s <- get

  put $ GameState $ getCount s + 1

  -- logDebug "WARNING: Hello World!"
  return ()


