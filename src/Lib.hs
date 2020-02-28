{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Lib where

import Control.Monad.State
import Control.Monad.Logger ( MonadLogger )
import Control.Monad.Logger.CallStack ( logDebug )
import Yael.Eff
import Yael.Eff.Log

data GameState = GameState { getCount :: Int } deriving Show

data State' s m = State'
  { _getState :: m s
  , _setState :: s -> m ()
  }

getState :: HasEff (State' s) f m => EffT f m s
getState = withEffT $ \State'{_getState} -> _getState

setState :: HasEff (State' s) f m => s -> EffT f m ()
setState s = withEffT $ \State'{_setState} -> _setState s

mtlState :: (MonadState s m) => State' s m
mtlState = State' {
    _getState = get
  , _setState = put
}

-- doesn't follow the contract of state
-- if we put, the get won't change
mockState :: (MonadState s m) => s -> State' s m
mockState mockState = State' {
    _getState = return mockState
  , _setState = \mockState -> return ()
}

-- TODO: IORef PassedIn
-- iorefState :: (MonadState s m, MonadIO m) => IORef -> State' s m
-- iorefState = State' {
--     _getState = return mockState
--   , _setState = \mockState -> return ()
-- }

stateWithLogging :: (Monad m, Show s) => (State' s m) -> (Log m) -> (State' s m)
stateWithLogging State'{ _getState = getState', _setState = setState' } Log{ _writeLog } = State' {
    _getState = do
      state <- getState'
      _writeLog $ "getting State" <> (show state)
      return state

  , _setState = \newState -> do
      _writeLog $ "setting State: " <> (show newState)
      setState' newState
}

newtype Testing = Testing { blah :: Int }

playGame :: (HasEff (State' GameState) f m, HasEff (State' Testing) f m, HasEff Log f m) => EffT f m ()
playGame = do
  s <- getState

  setState $ GameState $ getCount s + 1

  writeLog "WARNING: Hello World!"
  return ()


