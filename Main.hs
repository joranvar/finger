{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO)
import Control.Monad (forever)
import Data.Monoid ((<>))
import Data.Text.Lazy (Text)
import Data.Text.Lazy.IO as TIO (putStr, getLine)
import Data.Time (getCurrentTime)
import System.IO (BufferMode(NoBuffering), hSetBuffering, stdout)

import Types (AppState(..), Status(..))
import Web (startFingerServer)

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    state <- newTVarIO $ AppState []
    _ <- forkIO $ startFingerServer state
    forever $ inputLoop state

inputLoop :: TVar AppState -> IO ()
inputLoop state = do
    l <- prompt "Set a new label"
    content <- prompt $ "Set a new " <> l
    now <- getCurrentTime
    atomically $
        modifyTVar'
            state
            (\s ->
                 AppState $
                 Status l now content : status s)

prompt :: Text -> IO Text
prompt p = do
    TIO.putStr $ p <> ": "
    TIO.getLine
