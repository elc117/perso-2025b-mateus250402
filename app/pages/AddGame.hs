{-# LANGUAGE OverloadedStrings #-}

module Pages.AddGame where

import Lucid

addGamePage :: Html ()
addGamePage = html_ $ do
    head_ $ do
        title_ "Adicionar Jogo - Games Backlog"
        meta_ [charset_ "utf-8"]
    body_ $ do
        h1_ "Adicionar Jogo"
        form_ [method_ "post", action_ "/add"] $ do
            div_ $ do
                label_ "Nome do Jogo: "
                input_ [type_ "text", name_ "name", required_ ""]
            div_ $ do
                label_ "Nota (0-10): "
                input_ [type_ "number", name_ "score", min_ "0", max_ "10"]
            div_ $ do
                input_ [type_ "submit", value_ "Adicionar"]
        p_ $ a_ [href_ "/backlog"] "Voltar ao Backlog"