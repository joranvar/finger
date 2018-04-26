{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

module Web
    ( startFingerServer
    ) where

import Control.Concurrent.STM (TVar, atomically, modifyTVar', readTVarIO)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, ask, lift, runReaderT)
import Data.Text.Lazy (Text)
import Web.Scotty.Trans (ScottyT, get, json, scottyT)

import Types (AppState(..))

newtype WebM a = WebM
    { runWebM :: ReaderT (TVar AppState) IO a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadIO
               , MonadReader (TVar AppState)
               )

gets :: (AppState -> b) -> WebM b
gets f = f <$> (ask >>= liftIO . readTVarIO)

modify :: (AppState -> AppState) -> WebM ()
modify f = ask >>= liftIO . atomically . flip modifyTVar' f

startFingerServer :: TVar AppState -> IO ()
startFingerServer state =
    scottyT 13337 (\m -> runReaderT (runWebM m) state) actions

actions :: ScottyT Text WebM ()
actions =
    get "/finger" $ do
        currentStatus <- lift $ gets status
        json currentStatus
