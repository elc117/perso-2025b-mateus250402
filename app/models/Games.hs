{-# LANGUAGE DeriveGeneric #-}

module Models.Games where

import GHC.Generics
import Data.Text (Text)
import Database.SQLite.Simple.FromRow (FromRow(..), field)

data Game = Game
    { title :: Text
    , score :: Double
    , platform :: Text
    , cover_url :: Maybe Text
    } deriving (Show, Generic)

instance FromRow Game where
    fromRow = Game <$> field <*> field <*> field <*> field