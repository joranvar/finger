{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, DeriveGeneric,
  PartialTypeSignatures #-}
module Main (main) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (TVar, newTVarIO, readTVarIO, atomically, modifyTVar')
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT, ask, lift)
import Data.Aeson (ToJSON)
import Data.Monoid ((<>))
import Data.Time (UTCTime, getCurrentTime)
import Data.Text.Lazy (Text)
import Data.String (fromString)
import GHC.Generics (Generic)
import System.IO (BufferMode(NoBuffering), hSetBuffering, stdout)
import Web.Scotty.Trans (ActionT, scottyT, get, json)

data Status = Status
    { label :: Text
    , since :: UTCTime
    , contents :: Text
    } deriving (Generic)
instance ToJSON Status

newtype AppState = AppState
    { status :: [Status]
    }
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

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    state <- newTVarIO $ AppState []
    _ <-
        forkIO $
        scottyT
            13337
            (\m -> runReaderT (runWebM m) state) $
        get "/finger" $ do
            currentStatus <- lift $ gets status
            json currentStatus
                :: ActionT Text WebM ()
    forever $ do
        putStr "Set a new label: "
        l <- getLine
        putStr $ "Set a new " <> l <> ": "
        content <- getLine
        now <- getCurrentTime
        atomically $
            modifyTVar'
                state
                (\s ->
                     AppState $
                     Status (fromString l) now (fromString content) :
                     status s)
