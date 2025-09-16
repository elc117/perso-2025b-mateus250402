{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-} 

module Api.Igdb where

import Network.HTTP.Simple
import Data.Aeson
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Control.Exception (try, SomeException)
import Data.Maybe (mapMaybe)

data GameResponse = GameResponse
    { coverUrl :: Maybe T.Text
    } deriving (Show)

data GameResult = GameResult
    { grName :: T.Text
    , grPlatforms :: [T.Text]
    , grYear :: Maybe Int
    , grCoverUrl :: Maybe T.Text
    } deriving (Show, Eq)

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

searchMultipleGames :: T.Text -> IO [GameResult]
searchMultipleGames gameName = do
    let query = "fields name, platforms.name, first_release_date, cover.url; search \"" <> T.unpack gameName <> "\"; limit 20;"
    
    request' <- parseRequest "https://api.igdb.com/v4/games"
    let request = setRequestMethod "POST"
                $ setRequestHeader "Client-ID" ["poa6s33d3kywrcalk2xa52cs4h2bu2"]
                $ setRequestHeader "Authorization" ["Bearer babgqkix8mdd8ce0l2jl8he8cb3lnx"]
                $ setRequestHeader "Content-Type" ["text/plain"]
                $ setRequestBodyLBS (LBS.pack query) 
                $ request'
    
    response <- httpLBS request
    let responseBody = getResponseBody response
    
    case eitherDecode responseBody :: Either String [Value] of
        Left _ -> return []
        Right games -> return $ mapMaybe parseGameResultSafe games

parseGameResultSafe :: Value -> Maybe GameResult
parseGameResultSafe value = 
    case parseEither parseGameResult value of
        Left _ -> Nothing
        Right result -> Just result

parseGameResult :: Value -> Parser GameResult
parseGameResult = withObject "Game" $ \o -> do
    name <- o .: "name"
    platforms <- o .:? "platforms" .!= []
    platformNames <- mapM (\p -> withObject "Platform" (.: "name") p) platforms
    
    -- Converter timestamp Unix para ano
    releaseDate <- o .:? "first_release_date"
    let year = case releaseDate of
            Nothing -> Nothing
            Just timestamp -> Just $ timestampToYear timestamp
    
    cover <- o .:? "cover"
    coverUrl <- case cover of
        Nothing -> return Nothing
        Just coverObj -> do
            url <- withObject "Cover" (.: "url") coverObj
            let bigUrl = T.replace "t_thumb" "t_cover_big" url
            return $ Just $ "https:" <> bigUrl
    
    return $ GameResult name platformNames year coverUrl

-- Função helper para converter timestamp Unix para ano
timestampToYear :: Int -> Int
timestampToYear timestamp = 
    let secondsInYear = 365 * 24 * 60 * 60
        yearsSince1970 = timestamp `div` secondsInYear
    in 1970 + yearsSince1970

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