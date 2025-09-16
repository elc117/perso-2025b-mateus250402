{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Utils.Handles where

import Web.Scotty (ActionM, body, html, redirect, param, rescue)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Control.Monad.IO.Class (liftIO)
import Control.Exception (SomeException)
import Data.List (sortBy)
import Lucid (renderText)

-- Módulos internos
import qualified DB.DB as DB
import qualified Utils.Session as Session
import qualified Utils.Format as Format
import qualified Pages.Confirm as Confirm
import qualified Pages.Backlog as Backlog
import qualified Pages.Index as Index
import qualified Pages.Login as Login
import qualified Pages.Register as Register
import qualified Pages.AddGame as AddGame
import qualified Api.Igdb as Igdb
import qualified Utils.Data as Dt
import qualified Models.Games as Game
import qualified Pages.Selection as Selection

postLogin :: ActionM ()
postLogin = do
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

postRegister :: ActionM ()
postRegister = do
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

postAdd :: ActionM ()
postAdd = do
    requestBody <- body
    let formData = Format.parseFormData requestBody
    
    case (lookup "name" formData, lookup "score" formData, lookup "platform" formData) of
        (Just name, Just score, Just platform) -> do
            gameResults <- liftIO $ Igdb.searchMultipleGames (T.pack name)
            
            case gameResults of
                [] -> 
                    redirect $ TL.concat ["/confirm?name=", TL.pack name, "&score=", TL.pack score, "&platform=", TL.pack platform]
                
                [singleGame] -> 
                    redirect $ TL.concat 
                        [ "/confirm?name=", TL.fromStrict (Igdb.grName singleGame)
                        , "&score=", TL.pack score
                        , "&platform=", TL.pack platform
                        , "&cover_url=", maybe "" TL.fromStrict (Igdb.grCoverUrl singleGame)
                        ]
                
                multipleGames -> 
                    html $ renderText $ Selection.gameSelectionPage (T.pack name) (T.pack score) (T.pack platform) multipleGames
        
        _ -> html "Dados inválidos"

postConfirm :: ActionM ()
postConfirm = do
    requestBody <- body
    let formData = Format.parseFormData requestBody
    mUserId <- Session.sessionLookup "user_id"

    case (mUserId, lookup "name" formData, lookup "score" formData, lookup "platform" formData) of
        (Just userIdStr, Just name, Just score, Just platform) -> do
            let userId = read userIdStr :: Int
            let scoreDouble = read score :: Double
            
            -- Pegar a cover_url do formulário
            let maybeCoverUrl = case lookup "cover_url" formData of
                    Just "" -> Nothing
                    Just url -> Just (T.pack url)
                    Nothing -> Nothing

            result <- liftIO $ DB.insertGame userId (T.pack name) scoreDouble (T.pack platform) maybeCoverUrl
            case result of
                DB.Success _ -> redirect "/backlog"
                DB.Error msg -> html $ TL.pack $ "Erro ao salvar: " ++ msg
        _ -> html "Dados inválidos ou usuário não autenticado"

postDelete :: ActionM ()
postDelete = do
    gameId <- param "id"
    liftIO $ DB.deleteGame gameId
    redirect "/backlog"

getLogout :: ActionM ()
getLogout = do
    Session.sessionInsert "user_id" ""
    redirect "/"

getIndex :: ActionM ()
getIndex = do
    html $ renderText Index.indexPage

getLogin :: ActionM ()
getLogin = do
    html $ renderText Login.loginPage

getRegister :: ActionM ()
getRegister = do
    html $ renderText Register.registerPage

getAdd :: ActionM ()
getAdd = do
    html $ renderText AddGame.addGamePage

getConfirm :: ActionM ()
getConfirm = do
    name <- param "name"
    score <- param "score"
    platform <- param "platform"
    -- Pegar a cover_url que foi enviada via parâmetro GET
    maybeCoverParam <- (fmap Just (param "cover_url" :: ActionM TL.Text)) `rescue` (\(_ :: SomeException) -> return Nothing)
    
    let maybeCover = case maybeCoverParam of
            Just "" -> Nothing  -- Se for string vazia, considerar como Nothing
            Just url -> Just (TL.toStrict url)
            Nothing -> Nothing
    
    html $ renderText $ Confirm.confirmPage (TL.toStrict name) (TL.toStrict score) (TL.toStrict platform) maybeCover

getBacklog :: ActionM ()
getBacklog = do
    mUserId <- Session.sessionLookup "user_id"
    maybePlatform <- (fmap Just (param "platform" :: ActionM TL.Text)) `rescue` (\(_ :: SomeException) -> return Nothing)
    maybeSort <- (fmap Just (param "sort" :: ActionM TL.Text)) `rescue` (\(_ :: SomeException) -> return Nothing)
    maybeSearch <- (fmap Just (param "search" :: ActionM TL.Text)) `rescue` (\(_ :: SomeException) -> return Nothing)
    
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

getGameSelection :: ActionM ()
getGameSelection = do
    name <- param "name"
    score <- param "score"
    platform <- param "platform"  -- Adicionar este parâmetro
    gameResults <- liftIO $ Igdb.searchMultipleGames (TL.toStrict name)
    html $ renderText $ Selection.gameSelectionPage (TL.toStrict name) (TL.toStrict score) (TL.toStrict platform) gameResults