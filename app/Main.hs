{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty (scotty, get, post, param, html, redirect, ActionM, body)
import Lucid (renderText)
import Control.Monad.IO.Class (liftIO)
import qualified Pages.Index as Index
import qualified Pages.Login as Login
import qualified Pages.Register as Register
import qualified Pages.AddGame as AddGame
import qualified Pages.Backlog as Backlog
import qualified Pages.Confirm as Confirm
import qualified Data.Text.Lazy as TL      
import qualified Data.Text as T          
import qualified DB.DB as DB
import qualified Utils.Session as Session
import qualified Utils.Format as Format
import qualified Api.Igdb as Igdb 

main :: IO ()
main = do
    DB.initDB
    
    scotty 3000 $ do
        -- Página inicial
        get "/" $ do
            html $ renderText Index.indexPage

        -- Login
        get "/login" $ do
            html $ renderText Login.loginPage

        post "/login" $ do
            requestBody <- body
            let formData = Format.parseFormData requestBody

            case (lookup "email" formData, lookup "password" formData) of 
                (Just email, Just password) -> do
                    result <- liftIO $ DB.authenticateUser (T.pack email) (T.pack password)
                    case result of
                        DB.Success userId -> do
                            Session.sessionInsert "user_id" (show userId)
                            redirect "/"
                        DB.Error msg -> do
                            html $ TL.pack $ "Erro: " ++ msg
                _ -> do
                    html "Email ou senha incorretos"

        -- Logout
        get "/logout" $ do
            Session.sessionInsert "user_id" ""
            redirect "/"

        -- Registro
        get "/register" $ do
            html $ renderText Register.registerPage

        post "/register" $ do
            requestBody <- body
            let formData = Format.parseFormData requestBody

            case (lookup "email" formData, lookup "password" formData) of
                (Just email, Just password) -> do
                    result <- liftIO $ DB.insertUser (T.pack email) (T.pack password)
                    case result of
                        DB.Success userId -> redirect "/login"
                        DB.Error msg -> html $ TL.pack $ "Erro: " ++ msg
                _ -> do
                    html "Erro: email ou senha não encontrados"

        -- Adicionar jogo 
        get "/add" $ Session.requireAuth $ do
            html $ renderText AddGame.addGamePage

        post "/add" $ Session.requireAuth $ do
            requestBody <- body
            let formData = Format.parseFormData requestBody
            
            case (lookup "name" formData, lookup "score" formData, lookup "platform" formData) of
                (Just name, Just score, Just platform) -> do
                    let url = TL.concat ["/confirm?name=", TL.pack name, "&score=", TL.pack score, "&platform=", TL.pack platform]
                    redirect url
                _ -> do
                    html "Erro: dados do formulário incompletos"

        -- Confirmação
        get "/confirm" $ Session.requireAuth $ do
            name <- param "name"
            score <- param "score"
            platform <- param "platform"
            maybeCover <- liftIO $ Igdb.searchGameCover (TL.toStrict name)

            html $ renderText $ Confirm.confirmPage (TL.toStrict name) (TL.toStrict score) (TL.toStrict platform) maybeCover
        
        post "/confirm" $Session.requireAuth $ do
            requestBody <- body
            let formData = Format.parseFormData requestBody
            mUserId <- Session.sessionLookup "user_id"

            case (lookup "name" formData, lookup "score" formData, lookup "platform" formData, mUserId) of
                (Just name, Just score, Just platform, Just userIdStr) -> do
                    maybeCover <- liftIO $ Igdb.searchGameCover (T.pack name)
                    let coverUrl = maybe "" id maybeCover

                    _ <- liftIO $ DB.insertGame
                        (read userIdStr) -- user_id como Int
                        (T.pack name)
                        (read score :: Double)
                        (T.pack platform)
                        maybeCover
                    redirect "/backlog"
                _ -> html "Erro ao salvar jogo"

        -- Lista de jogos 
        get "/backlog" $ Session.requireAuth $ do
            html $ renderText Backlog.backlogPage   