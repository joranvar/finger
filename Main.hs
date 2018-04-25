{-# LANGUAGE OverloadedStrings, DeriveAnyClass, DeriveGeneric #-}
module Main where

import Data.Maybe (fromJust)
import Data.Time (UTCTime, parseTimeM, defaultTimeLocale)
import Data.Text (Text)
import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import Web.Scotty (scotty, get, json)

data Status = Status
    { label :: Text
    , since :: UTCTime
    , contents :: Text
    } deriving (Generic, ToJSON)

statuses :: [Status]
statuses =
    [ Status
          "main"
          (fromJust $ parseTimeM True defaultTimeLocale "%FT%X" "2018-04-25T15:37:00")
          "working with scotty"
    ]

main :: IO ()
main = scotty 13337 $ get "/finger" $ json statuses
