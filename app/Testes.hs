{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec
import Test.QuickCheck
import qualified Data.Text as T

import qualified Utils.Data as Data
import qualified Utils.Format as Format
import qualified DB.DB as DB
import Models.Games (Game(..))

testFilterGames :: Spec
testFilterGames = do
    describe "filterGames" $ do -- Título da suíte de testes

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
    describe "Database operations" $ do

        it "insert user" $ do
            let email = "test@example.com"
                password = "password123"
            result <- DB.insertUser email password
            case result of
                Right _ -> True `shouldBe` True  -- Sucesso esperado
                Left _  -> expectationFailure "Falha ao inserir usuário"
        
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
                Right _ -> expectationFailure "Usuário duplicado foi inserido, mas deveria falhar"
            _ <- DB.deleteUser (T.pack email) -- Limpeza
            return ()

main :: IO ()
main = hspec $ do
    testFilterGames
    testDB