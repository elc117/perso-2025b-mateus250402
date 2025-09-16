{-# LANGUAGE OverloadedStrings #-}

module Models.Games where

import Database.SQLite.Simple.FromRow
import Data.Text (Text)

data Game = Game
    { gameId :: Int
    , title :: Text
    , score :: Double
    , platform :: Text
    , cover_url :: Maybe Text
    } deriving (Show, Eq)

-- Implementação da instância FromRow para mapear resultados de consultas SQL para o tipo Game
instance FromRow Game where
    fromRow = Game <$> field <*> field <*> field <*> field <*> field -- id, title, score, platform, cover_url