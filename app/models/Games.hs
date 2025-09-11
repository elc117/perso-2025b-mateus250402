{-# LANGUAGE DeriveGeneric #-}

module Models.Game where

import GHC.Generics

data Game = Game
    { id :: Int
    , user_id :: Int
    , igdb_id :: Int
    , name :: Text
    , cover_url :: Text
    , release_date :: Text
    , score :: Float
    , platform :: Text}