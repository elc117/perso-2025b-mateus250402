{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty (scotty, get, post, param, html, redirect, ActionM, body, setHeader, request)
import Lucid (renderText)
import Control.Monad.IO.Class (liftIO)
import qualified Pages.Index as Index
import qualified Pages.Login as Login
import qualified Pages.Register as Register
import qualified Pages.AddGame as AddGame
import qualified Pages.Backlog as Backlog
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL      
import qualified Data.Text as T          
import qualified DB.DB as DB
import qualified Data.Text.Encoding as TE    
import Network.HTTP.Types.URI (parseQuery)   
import qualified Data.ByteString.Lazy as BSL 
import qualified Data.ByteString as BS     
import Web.Cookie (parseCookies, renderSetCookie, defaultSetCookie, setCookieName, setCookieValue, setCookieHttpOnly, setCookiePath)
import Network.Wai (requestHeaders)
import Blaze.ByteString.Builder (toLazyByteString)

-- Insere na sessão
sessionInsert :: String -> String -> ActionM ()
sessionInsert key value = do
    let cookie = defaultSetCookie 
            { setCookieName = TE.encodeUtf8 $ T.pack key
            , setCookieValue = TE.encodeUtf8 $ T.pack value
            , setCookiePath = Just "/"
            }
    setHeader "Set-Cookie" $ TL.fromStrict $ TE.decodeUtf8 $ BS.toStrict $ toLazyByteString $ renderSetCookie cookie

-- Busca na sessão
sessionLookup :: String -> ActionM (Maybe String)
sessionLookup key = do
    req <- request
    let headers = requestHeaders req
    case lookup "Cookie" headers of
        Nothing -> return Nothing
        Just cookieHeader -> do
            let cookies = parseCookies cookieHeader
                keyBS = TE.encodeUtf8 $ T.pack key
            case lookup keyBS cookies of
                Nothing -> return Nothing
                Just valueBS -> return $ Just $ T.unpack $ TE.decodeUtf8 valueBS

-- Verifica se está logado
requireAuth :: ActionM () -> ActionM ()
requireAuth action = do
    userId <- sessionLookup "user_id"
    case userId of
        Just "" -> redirect "/login"
        Just _ -> action
        Nothing -> redirect "/login"

-- Conversão do tipo ByteString para String ao receber dados do form
parseFormData :: BSL.ByteString -> [(String, String)]
parseFormData bodyLazy = 
    let bodyStrict = BSL.toStrict bodyLazy
        parsed = parseQuery bodyStrict
    in map (\(k, v) -> 
        let keyStr = T.unpack $ TE.decodeUtf8 k 
            valStr = maybe "" (T.unpack . TE.decodeUtf8) v  
        in (keyStr, valStr)
    ) parsed

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
            let formData = parseFormData requestBody

            case (lookup "email" formData, lookup "password" formData) of 
                (Just email, Just password) -> do
                    let emailStrict = T.pack email
                    let passwordStrict = T.pack password

                    result <- liftIO $ DB.authenticateUser emailStrict passwordStrict
                    case result of
                        DB.Success userId -> do
                            sessionInsert "user_id" (show userId)
                            redirect "/"
                        DB.Error msg -> do
                            html $ TL.pack $ "Erro: " ++ msg
                _ -> do
                    html "Email ou senha incorretos"

                    
        -- Logout
        get "/logout" $ do
            sessionInsert "user_id" ""
            redirect "/"


        -- Registro
        get "/register" $ do
            html $ renderText Register.registerPage

        post "/register" $ do
            requestBody <- body
            let formData = parseFormData requestBody

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
        get "/add" $ requireAuth $ do
            html $ renderText AddGame.addGamePage

        post "/add" $ requireAuth $ do
            name <- param "name" :: ActionM Text
            score <- param "score" :: ActionM Text
            userId <- sessionLookup "user_id" 
            liftIO $ putStrLn $ "Usuário " ++ show userId ++ " adicionando jogo: " ++ show name
            redirect "/backlog"

        
        -- Lista de jogos 
        get "/backlog" $ requireAuth $ do
            html $ renderText Backlog.backlogPage