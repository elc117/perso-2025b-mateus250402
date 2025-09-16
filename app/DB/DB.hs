{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DB.DB where

import Database.SQLite.Simple
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE  
import qualified Crypto.Hash as Hash    
import qualified Data.ByteString.Base64 as B64  
import Control.Exception (try, SomeException) 
import qualified Data.ByteArray as BA
import Models.Games (Game)


dbPath :: String
dbPath = "backlog.db"

connectDB :: IO Connection
connectDB = open dbPath

-- Inicializa o banco de dados, criando tabelas se não existirem
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
        , "  title TEXT NOT NULL,"
        , "  cover_url TEXT,"
        , "  score REAL,"
        , "  platform TEXT,"
        , "  FOREIGN KEY (user_id) REFERENCES users (id)"
        , ")"
        ]
    
    close conn  

-- Função para hashear a senha usando SHA256 e codificar em Base64
hashPassword :: Text -> Text
hashPassword password = 
    let passwordBytes = TE.encodeUtf8 password
        digest = Hash.hash passwordBytes :: Hash.Digest Hash.SHA256
        digestBytes = BA.convert digest 
        encoded = B64.encode digestBytes
    in TE.decodeUtf8 encoded



insertUser :: Text -> Text -> IO (Either String Int) -- Retorna Sring em caso de erro ou Int (userId) em caso de sucesso
insertUser email password = do
    let hashedPassword = hashPassword password

    result <- (try $ do
        conn <- connectDB
        execute conn "INSERT INTO users (email, password_hash) VALUES (?, ?)"  
            (email, hashedPassword)
        userId <- lastInsertRowId conn
        close conn
        return (fromIntegral userId)) :: IO (Either SomeException Int) -- Pode ser um erro de exceção ou o userId

    case result of
        Right userId -> return $ Right userId  
        Left err -> return $ Left $ "Erro ao inserir usuário: " ++ show err

insertGame :: Int -> Text -> Double -> Text -> Maybe Text -> IO (Either String Int) -- Retorna String em caso de erro ou Int (gameId) em caso de sucesso
insertGame userId title score platform maybeCoverUrl = do
    conn <- connectDB

    result <- try $ do
        execute conn "INSERT INTO games (user_id, title, score, platform, cover_url) VALUES (?, ?, ?, ?, ?)" 
                (userId, title, score, platform, maybeCoverUrl)
        lastId <- lastInsertRowId conn
        close conn
        return $ fromIntegral lastId

    case result of
        Right gameId -> return $ Right gameId
        Left (_ :: SomeException) -> return $ Left "Erro ao inserir jogo no banco de dados"
        
getGames :: Int -> IO [Game]
getGames user_id = do
    conn <- connectDB
    games <- query conn "SELECT id, title, score, platform, cover_url FROM games WHERE user_id = ? ORDER BY title ASC" (Only user_id) 
    close conn
    return games

deleteGame :: Int -> IO ()
deleteGame gameId = do
    conn <- connectDB
    execute conn "DELETE FROM games WHERE id = ?" (Only gameId)
    close conn

authenticateUser :: Text -> Text -> IO (Either String Int) 
authenticateUser email password = do
    let hashedPassword = hashPassword password

    result <- (try $ do
        conn <- connectDB
        row <- query conn "SELECT id, password_hash FROM users WHERE email = ?" (Only email)
        close conn

        case row of
            [] -> return Nothing -- Linha vazia, usuário não encontrado
            (userId, storedHash):_ ->
                if storedHash == hashedPassword
                    then return (Just userId)
                    else return Nothing
        ) :: IO (Either SomeException (Maybe Int)) -- Pode ser um erro de exceção ou Maybe Int (userId)

    case result of
        Right (Just userId) -> return $ Right userId -- Funcionou e encontrou o usuário
        Right Nothing -> return $ Left "E-mail ou senha incorretos" -- Funcionou, mas não encontrou ou senha incorreta
        Left err -> return $ Left $ "Erro no DB" ++ show err 