{-# LANGUAGE OverloadedStrings #-}

module Pages.Confirm where

import Lucid
import Data.Text (Text)

confirmPage :: Text -> Text -> Text -> Html ()
confirmPage name score platform  = html_ $ do
    head_ $ do
        title_ "Confirmar Jogo - Games Backlog"
        meta_ [charset_ "utf-8"]
    body_ $ do
        h1_ "Confirmar Jogo"
        p_ $ "Nome: " <> toHtml name
        p_ $ "Nota: " <> toHtml score
        p_ $ "Plataforma: " <> toHtml platform