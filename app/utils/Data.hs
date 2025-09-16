{-# LANGUAGE OverloadedStrings #-}

module Utils.Data where

import qualified Data.Text as T
import Models.Games (Game(..))

filterGames :: [Game] -> T.Text -> T.Text -> [Game]
filterGames games platformFilter searchFilter =
    let platformFiltered = if T.null platformFilter
                            then games
                            else filter (\game -> platform game == platformFilter) games
        
        searchFiltered = if T.null searchFilter
                         then platformFiltered
                         else filter (\game -> T.toLower searchFilter `T.isInfixOf` T.toLower (title game)) platformFiltered
    in searchFiltered