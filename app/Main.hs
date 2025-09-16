{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty (scotty, get, post)

-- Utils
import qualified DB.DB as DB
import qualified Utils.Session as Session
import qualified Utils.Handles as Hd

main :: IO ()
main = do
    DB.initDB
    
    scotty 3000 $ do

        get "/" Hd.getIndex

        get "/login" Hd.getLogin
        post "/login" Hd.postLogin

        get "/logout" Hd.getLogout

        get "/register" Hd.getRegister
        post "/register" Hd.postRegister

        get "/add" $ Session.requireAuth $ do Hd.getAdd
        post "/add" $ Session.requireAuth $ do Hd.postAdd

        get "/backlog" $ Session.requireAuth $ do Hd.getBacklog

        get "/game-selection" $ Session.requireAuth $ do Hd.getGameSelection

        get "/confirm" $ Session.requireAuth $ do Hd.getConfirm
        post "/confirm" $ Session.requireAuth $ do Hd.postConfirm

        post "/delete/:id" $ Session.requireAuth $ do Hd.postDelete
            