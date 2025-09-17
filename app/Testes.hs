{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec
import Test.QuickCheck
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as LBS

import qualified Utils.Data as Data
import qualified Utils.Format as Format
import qualified DB.DB as DB
import qualified Api.Igdb as Igdb
import Models.Games (Game(..))

testFilterGames :: Spec
testFilterGames = do
    describe "========== Filter Tests ==========" $ do -- Título da suíte de testes

        let games = [ Game 1 "God of War" 9 "Playstation" Nothing
                    , Game 2 "Halo" 8 "Xbox" Nothing
                    , Game 3 "Mario Kart" 7 "Nintendo" Nothing
                    , Game 4 "Cyberpunk 2077" 10 "PC" Nothing
                    ]

        it "no filters" $ do -- Caso de teste
            let result = Data.filterGames games "" ""
            length result `shouldBe` 4
        
        it "filter by platform" $ do
            let result = Data.filterGames games "Playstation" ""
            length result `shouldBe` 1
            title (head result) `shouldBe` "God of War"

        it "filter by title" $ do
            let result = Data.filterGames games "" "Halo"
            length result `shouldBe` 1
            title (head result) `shouldBe` "Halo"
        
        it "filter by platform and title" $ do
            let result = Data.filterGames games "PC" "Cyberpunk"
            length result `shouldBe` 1
            title (head result) `shouldBe` "Cyberpunk 2077"

        it "no matches" $ do
            let result = Data.filterGames games "Xbox" "Mario Kart"
            length result `shouldBe` 0

testDB :: Spec
testDB = do
    describe "========== Database Tests ==========" $ do

        it "insert user" $ do
            let email = "test@example.com"
                password = "password123"
            result <- DB.insertUser email password
            case result of
                Right _ -> True `shouldBe` True  -- Sucesso esperado
                Left _  -> expectationFailure "Failed to insert user"

        it "hash password" $ do
            let password = "password123"
            let hashed1 = DB.hashPassword (T.pack password)
            let hashed2 = DB.hashPassword (T.pack password)

            hashed1 `shouldBe` hashed2
            hashed1 `shouldNotBe` T.pack password

        it "authenticate user" $ do
            let email = "test@example.com"
                password = "password123"
            result <- DB.authenticateUser (T.pack email) (T.pack password)
            case result of
                Right _ -> True `shouldBe` True  -- Sucesso esperado
                Left _  -> expectationFailure "Failed to authenticate user"

        it "delete user" $ do
            let email = "test@example.com"
            delResult <- DB.deleteUser (T.pack email)
            delResult `shouldBe` True 

        it "not insert duplicate user" $ do
            let email = "repeated@example.com"
                password = "password123"
            _ <- DB.insertUser (T.pack email) (T.pack password) -- Primeiro inserção deve ser bem-sucedida
            result <- DB.insertUser (T.pack email) (T.pack password)
            case result of
                Left _  -> True `shouldBe` True  -- Esperado: falha ao tentar inserir duplicado
                Right _ -> expectationFailure "Duplicate user insertion should fail"
            _ <- DB.deleteUser (T.pack email) -- Limpeza
            return ()

        it "insert and delete game" $ do
            result <- DB.insertGame 999 (T.pack "The Witcher 3") 10 (T.pack "PC") (Just (T.pack "https://images.igdb.com/igdb/image/upload/t_cover_big/co1wyy.jpg"))
            case result of
                Right gameId -> DB.deleteGame gameId
                Left _  -> expectationFailure "Failed to insert game"
            return ()
        
        it "get games for user" $ do
            let userId = 999
            r1 <- DB.insertGame userId (T.pack "The Witcher 3") 10 (T.pack "PC") (Just (T.pack "https://images.igdb.com/igdb/image/upload/t_cover_big/co1wyy.jpg"))
            r2 <- DB.insertGame userId (T.pack "Minecraft") 9 (T.pack "PC") (Just (T.pack "https://images.igdb.com/igdb/image/upload/t_cover_big/co8fu7.jpg"))
            r3 <- DB.insertGame userId (T.pack "Baldur's Gate 3") 10 (T.pack "PC") (Just (T.pack "https://images.igdb.com/igdb/image/upload/t_cover_big/co670h.jpg"))
            case (r1, r2, r3) of
                (Right gameId1, Right gameId2, Right gameId3) -> do
                    games <- DB.getGames userId
                    length games `shouldBe` 3
                    map title games `shouldContain` ["Baldur's Gate 3", "Minecraft", "The Witcher 3"]
                    mapM_ DB.deleteGame [gameId1, gameId2, gameId3]
                _ -> expectationFailure "Failed to insert games"         
            return ()

testApi :: Spec
testApi = do
    describe "========== API Tests ==========" $ do

        it "search games" $ do
            let gameName = (T.pack "Call of Duty")
            results <- Igdb.searchMultipleGames gameName
            length results `shouldSatisfy` (> 0) -- Sucesso aqui indica que parseGameResult e parseGameResultSafe executaram com sucesso

testFormat :: Spec
testFormat = do
    describe "========== Format Tests ==========" $ do
        it "timestamp to year" $ do
            let timeStamp = 1758132254 -- Exemplo de timestamp
            let year = Format.timestampToYear timeStamp
            year `shouldBe` 2025

        it "parse form data" $ do
            let formData = "email=test%40example.com&password=secret"
            let parsed = Format.parseFormData (LBS.pack formData)
            parsed `shouldBe` [("email", "test@example.com"), ("password", "secret")]

main :: IO ()
main = hspec $ do
    testFilterGames
    testDB
    testApi
    testFormat