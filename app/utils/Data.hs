{-# LANGUAGE OverloadedStrings #-}

module Utils.Data where

import qualified Data.Text as T
import qualified Models.Games as Game
import Data.List (sortBy)

filterGames :: [Game.Game] -> T.Text -> T.Text -> [Game.Game]
filterGames games platformFilter searchFilter = 
    let platformFiltered = if platformFilter == ""
                          then games
                          else filter (\g -> Game.platform g == platformFilter) games
        searchFiltered = if searchFilter == ""
                        then platformFiltered
                        else filter (\g -> T.isInfixOf (T.toLower searchFilter) (T.toLower (Game.title g))) platformFiltered
    in searchFiltered