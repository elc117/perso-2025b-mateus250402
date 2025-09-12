{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty (scotty, get, post, param, html, redirect, ActionM, body, setHeader, request)
import Lucid (renderText)
import Control.Monad.IO.Class (liftIO)
import Data.Text.Lazy (Text)
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
                    let emailStrict = T.pack email
                    let passwordStrict = T.pack password

                    result <- liftIO $ DB.authenticateUser emailStrict passwordStrict
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
                    let emailStrict = T.pack email  
                    let passwordStrict = T.pack password
                    
                    result <- liftIO $ DB.insertUser emailStrict passwordStrict
                    
                    case result of
                        DB.Success userId -> do
                            redirect "/login"
                        DB.Error msg -> do
                            html $ TL.pack $ "Erro: " ++ msg
                
                _ -> do
                    html "Erro: email ou senha nao encontrados"


        -- Adicionar jogo 
        get "/add" $ Session.requireAuth $ do
            html $ renderText AddGame.addGamePage

        post "/add" $ Session.requireAuth $ do
            requestBody <- body
            let formData = Format.parseFormData requestBody
            userId <- Session.sessionLookup "user_id"
            
            case (lookup "name" formData, lookup "score" formData, lookup "platform" formData) of
                (Just name, Just score, Just platform) -> do
                    let nameText = TL.pack name
                        scoreText = TL.pack score
                        platformText = TL.pack platform
                        url = "/confirm?name=" <> nameText <> "&score=" <> scoreText <> "&platform=" <> platformText
                    redirect url
                _ -> do
                    html "Erro: dados do formulário incompletos"

        
        -- Confirmação
        get "/confirm" $ Session.requireAuth $ do
            nameL <- param "name" :: ActionM Text
            scoreL <- param "score" :: ActionM Text
            platformL <- param "platform" :: ActionM Text
            let name = TL.toStrict nameL
                score = TL.toStrict scoreL
                platform = TL.toStrict platformL
            html $ renderText $ Confirm.confirmPage name score platform

        
        -- Lista de jogos 
        get "/backlog" $ Session.requireAuth $ do
            html $ renderText Backlog.backlogPage