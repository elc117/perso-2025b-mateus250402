{-# LANGUAGE OverloadedStrings #-}

module Pages.Confirm where

import Lucid

confirmPage :: Html ()
confirmPage = html_ $ do
    head_ $ do
        title_ "Confirmar Jogo - Games Backlog"
        meta_ [charset_ "utf-8"]
    body_ $ do
        h1_ "Confirmar Jogo"