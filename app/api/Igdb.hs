{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-} 

module Api.Igdb where

import Network.HTTP.Simple
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Control.Exception (try, SomeException)

data GameResponse = GameResponse
    { coverUrl :: Maybe T.Text
    } deriving (Show)

instance FromJSON GameResponse where
    parseJSON = withObject "GameResponse" $ \o -> do
        cover <- o .:? "cover"
        case cover of
            Nothing -> return $ GameResponse Nothing
            Just coverObj -> do
                url <- coverObj .: "url"
                let bigUrl = T.replace "t_thumb" "t_cover_big" url
                return $ GameResponse (Just $ "https:" <> bigUrl)

-- Busca apenas a URL da capa do jogo
searchGameCover :: T.Text -> IO (Maybe T.Text)
searchGameCover name = do
    result <- try $ makeIgdbRequest name
    case result of
        Left (_ :: SomeException) -> return Nothing
        Right coverUrl -> return coverUrl

makeIgdbRequest :: T.Text -> IO (Maybe T.Text)
makeIgdbRequest name = do
    let query = "fields cover.url; search \"" <> T.unpack name <> "\"; limit 1;"
    
    request' <- parseRequest "https://api.igdb.com/v4/games"
    let request = setRequestMethod "POST"
                $ setRequestHeader "Client-ID" ["poa6s33d3kywrcalk2xa52cs4h2bu2"]
                $ setRequestHeader "Authorization" ["Bearer babgqkix8mdd8ce0l2jl8he8cb3lnx"]
                $ setRequestHeader "Content-Type" ["text/plain"]
                $ setRequestBodyLBS (LBS.pack query) 
                $ request'
    
    response <- httpLBS request
    let responseBody = getResponseBody response
    
    case eitherDecode responseBody :: Either String [GameResponse] of
        Left _ -> return Nothing
        Right [] -> return Nothing
        Right (game:_) -> return $ coverUrl game