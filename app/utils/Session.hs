{-# LANGUAGE OverloadedStrings #-}

module Utils.Session
  ( sessionInsert
  , sessionLookup
  , requireAuth
  ) where

import Web.Scotty (ActionM, setHeader, redirect, request)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import Blaze.ByteString.Builder (toLazyByteString)
import Web.Cookie (parseCookies, renderSetCookie, defaultSetCookie, setCookieName, setCookieValue, setCookiePath)
import Network.Wai (requestHeaders)

-- Insere na sessão
sessionInsert :: String -> String -> ActionM ()
sessionInsert key value = do
    let cookie = defaultSetCookie 
            { setCookieName = TE.encodeUtf8 $ T.pack key -- Converte String -> Text -> ByteString que é o formato esperado pela função
            , setCookieValue = TE.encodeUtf8 $ T.pack value
            , setCookiePath = Just "/" -- Cookie válido para todos os caminhos
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