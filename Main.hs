{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Main
    ( main
    ) where

import Brick
    ( BrickEvent(..)
    , EventM(..)
    , Next
    , Widget(..)
    , attrMap
    , continue
    , str
    , handleEventLensed
    , (<+>)
    , (<=>)
    )
import qualified Brick.Widgets.Border as B (borderWithLabel)
import qualified Brick.Widgets.Edit as E
    ( Editor(..)
    , editor
    , getEditContents
    , handleEditorEvent
    , renderEditor
    )
import qualified Brick.Widgets.List as L (list, renderList)
import Brick.Main (App(..), defaultMain, showCursorNamed)
import Control.Concurrent (forkIO, yield)
import Control.Concurrent.STM
    ( TVar
    , atomically
    , modifyTVar'
    , newTVarIO
    , readTVarIO
    )
import Control.Lens.TH (makeLenses)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (toJSON)
import Data.Text (Text)
import qualified Data.Text as T (unpack, unlines)
import Data.Time (getCurrentTime)
import qualified Data.Vector as V (fromList)
import qualified Graphics.Vty as V (Event(EvKey), Key(..), Modifier(..), defAttr)
import System.IO (BufferMode(NoBuffering), hSetBuffering, stdout)

import Types (AppState(..), Status(..))
import Web (startFingerServer)

data UIState = UIState { _s :: TVar AppState
                       , _labelEditor :: E.Editor Text Int
                       , _contentEditor :: E.Editor Text Int
                       , _focusedEditor :: Int
                       , _statuses :: [Status]
                       }
makeLenses ''UIState

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    state <- newTVarIO $ AppState []
    _serverHandle <- forkIO $ startFingerServer state
    yield
    _finalState <-
        defaultMain
            App
                { appDraw = drawApp
                , appStartEvent = pure
                , appChooseCursor = showCursorNamed . _focusedEditor
                , appHandleEvent = handleAppEvent
                , appAttrMap = const (attrMap V.defAttr [])
                } $
        UIState state (E.editor 1 (Just 1) "") (E.editor 2 Nothing "") 1 []
    pure ()

drawApp :: UIState -> [Widget Int]
drawApp ui =
    [ (B.borderWithLabel
           (str "Set your status")
           (str "Label: " <+>
            E.renderEditor
                (str . T.unpack . head)
                (_focusedEditor ui == 1)
                (_labelEditor ui) <+>
            str "Content: " <+>
            E.renderEditor
                (str . T.unpack . head)
                (_focusedEditor ui == 2)
                (_contentEditor ui))) <=>
      (B.borderWithLabel
           (str "Current statuses")
           (L.renderList
                (const (str . show . toJSON))
                False
                (L.list 3 (V.fromList $ _statuses ui) 1)))
    ]

handleAppEvent :: UIState -> BrickEvent Int e -> EventM Int (Next UIState)
handleAppEvent ui (VtyEvent (V.EvKey key mods))
    | key == V.KChar '\t' && _focusedEditor ui == 1 =
        continue $ ui {_focusedEditor = 2}
    | key == V.KChar '\t' && _focusedEditor ui == 2 =
        continue $ ui {_focusedEditor = 1}
    | key == V.KBackTab && _focusedEditor ui == 1 =
        continue $ ui {_focusedEditor = 2}
    | key == V.KBackTab && _focusedEditor ui == 2 =
        continue $ ui {_focusedEditor = 1}
    | key == V.KEnter && V.MMeta `elem` mods = do
        statuses' <- liftIO $ inputLoop (_s ui) (mconcat $ E.getEditContents $ _labelEditor ui) (T.unlines $ E.getEditContents $ _contentEditor ui)
        continue $ UIState (_s ui) (E.editor 1 (Just 1) "") (E.editor 2 Nothing "") 1 statuses'
handleAppEvent ui (VtyEvent e)
    | _focusedEditor ui == 1 =
        continue =<< handleEventLensed ui labelEditor E.handleEditorEvent e
    | _focusedEditor ui == 2 =
        continue =<< handleEventLensed ui contentEditor E.handleEditorEvent e
handleAppEvent ui _ = continue ui

inputLoop :: TVar AppState -> Text -> Text -> IO [Status]
inputLoop state l content = do
    now <- getCurrentTime
    atomically $
        modifyTVar'
            state
            (\s' -> AppState $ Status l now (nonEmpty content) : status s')
    status <$> readTVarIO state

nonEmpty :: Text -> Maybe Text
nonEmpty t =
    if t == ""
        then Nothing
        else Just t
