{-# LANGUAGE DeriveGeneric #-}

module Models.Game where

import GHC.Generics

data User = User
        { id :: Int
        , email :: Text
        ,password_hash :: Text}