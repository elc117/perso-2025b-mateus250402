{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Web.Scotty (scotty, get, post, param, html, redirect, ActionM, body, rescue)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL      
import Lucid (renderText)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (SomeException)
import Data.List (sortBy)

-- Pages
import qualified Pages.Index as Index
import qualified Pages.Login as Login
import qualified Pages.Register as Register
import qualified Pages.AddGame as AddGame
import qualified Pages.Backlog as Backlog
import qualified Pages.Confirm as Confirm

-- Utils e Models
import qualified DB.DB as DB
import qualified Utils.Session as Session
import qualified Utils.Format as Format
import qualified Utils.Data as Dt
import qualified Api.Igdb as Igdb 
import qualified Models.Games as Game

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
            mUserId <- Session.sessionLookup "user_id"
            maybePlatform <- (fmap Just (param "platform" :: ActionM TL.Text)) `rescue` ((\(_ :: SomeException) -> return Nothing) :: SomeException -> ActionM (Maybe TL.Text))
            maybeSort <- (fmap Just (param "sort" :: ActionM TL.Text)) `rescue` ((\(_ :: SomeException) -> return Nothing) :: SomeException -> ActionM (Maybe TL.Text))
            maybeSearch <- (fmap Just (param "search" :: ActionM TL.Text)) `rescue` ((\(_ :: SomeException) -> return Nothing) :: SomeException -> ActionM (Maybe TL.Text))
            
            let platformFilter = case maybePlatform of
                    Nothing -> ""
                    Just p -> TL.toStrict p
            
            let searchFilter = case maybeSearch of
                    Nothing -> ""
                    Just s -> TL.toStrict s
            
            let sortByScore = case maybeSort of
                    Just "score" -> True
                    _ -> False

            case mUserId of
                Just userIdStr -> do
                    let userId = read userIdStr :: Int
                    allGames <- liftIO $ DB.getGames userId

                    let filteredGames = Dt.filterGames allGames platformFilter searchFilter

                    let sortedGames = if sortByScore
                                     then sortBy (\a b -> compare (Game.score b) (Game.score a)) filteredGames
                                     else filteredGames
                        
                    html $ renderText $ Backlog.backlogPage sortedGames

                Nothing -> redirect "/login"

        
        post "/delete/:titulo" $ Session.requireAuth $ do
            title <- param "titulo"
            maybeIdUser <- Session.sessionLookup "user_id"
            case maybeIdUser of
                Just idUserStr -> do
                    let idUser = read idUserStr
                    liftIO $ DB.deleteGame idUser title
                    redirect "/backlog"
                Nothing -> redirect "/login"