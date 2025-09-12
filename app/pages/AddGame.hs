{-# LANGUAGE OverloadedStrings #-}

module Pages.AddGame where

import Lucid

addGamePage :: Html ()
addGamePage = html_ $ do
    head_ $ do
        title_ "Adicionar Jogo - Games Backlog"
        meta_ [charset_ "utf-8"]
        link_ [ rel_ "stylesheet" , href_ "https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/css/bootstrap.min.css"]


    nav_ [class_ "navbar navbar-expand-lg navbar-dark bg-primary"] $ do
            div_ [class_ "container"] $ do
                a_ [class_ "navbar-brand", href_ "/"] "🎮 Games Backlog"
                div_ [class_ "navbar-nav ms-auto"] $ do
                    a_ [class_ "nav-link", href_ "/"] "Início"
                    a_ [class_ "nav-link", href_ "/backlog"] "Backlog"
                    a_ [class_ "nav-link", href_ "/logout"] "Logout"
    
    
    body_ [class_ "bg-light"] $ do
        div_ [class_ "container mt-5"] $ do
            h1_ [class_ "mb-4"] "Adicionar Jogo"
            form_ [method_ "post", action_ "/add", class_ "card p-4 shadow-sm"] $ do
                div_ [class_ "mb-3"] $ do
                    label_ [class_ "form-label"] "Nome do Jogo: "
                    input_ [type_ "text", name_ "name", required_ "", class_ "form-control"]
                div_ [class_ "mb-3"] $ do
                    label_ [class_ "form-label"] "Nota (0-10): "
                    input_ [type_ "number", name_ "score", min_ "0", max_ "10", class_ "form-control"]
                div_ [class_ "mb-3"] $ do
                    label_ [class_ "form-label"] "Plataforma: "
                    select_ [name_ "platform", required_ "", class_ "form-select"] $ do
                        option_ [value_ "PC"] "PC"
                        option_ [value_ "PlayStation"] "PlayStation"
                        option_ [value_ "Xbox"] "Xbox"
                        option_ [value_ "Nintendo"] "Nintendo"
                button_ [type_ "submit", class_ "btn btn-primary"] "Adicionar"
            p_ [class_ "mt-3"] $ a_ [href_ "/backlog", class_ "btn btn-link"] "Voltar ao Backlog"