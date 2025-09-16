{-# LANGUAGE OverloadedStrings #-}

module Models.Games where

import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple
import Data.Text (Text)

data Game = Game
    { gameId :: Int
    , title :: Text
    , score :: Double
    , platform :: Text
    , cover_url :: Maybe Text
    } deriving (Show, Eq)

instance FromRow Game where
    fromRow = Game <$> field <*> field <*> field <*> field <*> field