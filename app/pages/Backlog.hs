{-# LANGUAGE OverloadedStrings #-}

module Pages.Backlog where

import Lucid

backlogPage :: Html ()
backlogPage = html_ $ do
    head_ $ do
        title_ "Meu Backlog - Games Backlog"
        meta_ [charset_ "utf-8"]
    body_ $ do
        h1_ "Meu Backlog"
        div_ $ do
            a_ [href_ "/add"] "Adicionar Jogo"
            " | "
            a_ [href_ "/"] "Home"
        h2_ "Jogos Salvos"
        p_ "Aqui aparecerão seus jogos..."