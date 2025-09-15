{-# LANGUAGE OverloadedStrings #-}

module DB.DB where

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE  
import qualified Crypto.Hash as Hash    
import qualified Data.ByteString.Base64 as B64  
import Control.Exception (try, SomeException) 
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteArray as BA
import Models.Games (Game)

data Result a = Success a | Error String deriving (Show, Eq)

dbPath :: String
dbPath = "backlog.db"

connectDB :: IO Connection
connectDB = open dbPath

initDB :: IO ()
initDB = do
    conn <- connectDB

    execute_ conn $ Query $ T.pack $ unlines
        [ "CREATE TABLE IF NOT EXISTS users ("
        , "  id INTEGER PRIMARY KEY AUTOINCREMENT,"
        , "  email TEXT UNIQUE NOT NULL," 
        , "  password_hash TEXT NOT NULL" 
        , ")"
        ]

    execute_ conn $ Query $ T.pack $ unlines
        [ "CREATE TABLE IF NOT EXISTS games ("
        , "  id INTEGER PRIMARY KEY AUTOINCREMENT,"
        , "  user_id INTEGER NOT NULL,"
        , "  igdb_id INTEGER,"
        , "  name TEXT NOT NULL,"
        , "  cover_url TEXT,"
        , "  release_date TEXT,"
        , "  score REAL,"
        , "  platform TEXT,"
        , "  FOREIGN KEY (user_id) REFERENCES users (id)"
        , ")"
        ]
    
    close conn  

hashPassword :: Text -> Text
hashPassword password = 
    let passwordBytes = TE.encodeUtf8 password
        digest = Hash.hash passwordBytes :: Hash.Digest Hash.SHA256
        digestBytes = BA.convert digest 
        encoded = B64.encode digestBytes
    in TE.decodeUtf8 encoded

insertUser :: Text -> Text -> IO (Result Int)
insertUser email password = do
    let hashedPassword = hashPassword password

    result <- (try $ do
        conn <- connectDB
        execute conn "INSERT INTO users (email, password_hash) VALUES (?, ?)"  
            (email, hashedPassword)
        userId <- lastInsertRowId conn
        close conn
        return (fromIntegral userId)) :: IO (Either SomeException Int)

    case result of
        Right userId -> return $ Success userId 
        Left err -> return $ Error $ "Erro ao inserir usuário: " ++ show err

insertGame :: Int -> Text -> Double -> Text -> Maybe Text -> IO (Result Int)
insertGame user_id title score platform cover_url = do
    result <- (try $do
            conn <- connectDB
            execute conn "INSERT INTO games (user_id, title, score, platform, cover_url) VALUES (?, ?, ?, ?, ?)"
                (user_id, title, score, platform, cover_url)
            gameId <- lastInsertRowId conn
            close conn
            return (fromIntegral gameId)) :: IO (Either SomeException Int)

    case result of
        Right gameId -> return $ Success gameId
        Left err -> return $ Error $ "Erro ao inserir game: " ++ show err

getGames :: Int -> IO [Game]
getGames user_id = do
        conn <- connectDB
        games <- query conn "SELECT title, score, platform, cover_url FROM games WHERE user_id = ?" (Only user_id)
        close conn
        return games

authenticateUser :: Text -> Text -> IO (Result Int)
authenticateUser email password = do
    let hashedPassword = hashPassword password

    result <- (try $ do
        conn <- connectDB
        row <- query conn "SELECT id, password_hash FROM users WHERE email = ?" (Only email) -- query espera uma tupla, por isso o Only
        close conn

        case row of
            [] -> return Nothing -- Linha vazia, usuário não encontrado (Maybe Int)
            (userId, storedHash):_ ->
                if storedHash == hashedPassword
                    then return (Just userId) -- Maybe Int
                    else return Nothing -- Maybe Int
        ) :: IO (Either SomeException (Maybe Int)) -- Indica que o retorno pode ser um Exceção ou um Int (id do usuário)

    case result of
        Right (Just userId) -> return $ Success userId
        Right Nothing -> return $ Error "E-mail ou senha incorretos"
        Left err -> return $ Error $ "Erro no DB" ++ show err