{-# LANGUAGE DeriveGeneric #-}

module Types
    ( AppState(..)
    , Status(..)
    ) where

import Data.Aeson (ToJSON)
import Data.Text.Lazy (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

data Status = Status
    { label :: Text
    , since :: UTCTime
    , contents :: Maybe Text
    } deriving (Generic)
instance ToJSON Status

newtype AppState = AppState
    { status :: [Status]
    }
