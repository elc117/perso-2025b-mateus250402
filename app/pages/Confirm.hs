{-# LANGUAGE OverloadedStrings #-}

module Pages.Confirm where

import Lucid
import Data.Text (Text)

confirmPage :: Text -> Text -> Text -> Maybe Text -> Html ()
confirmPage name score platform maybeCover = html_ $ do
    head_ $ do
        title_ "Confirmar Jogo - Games Backlog"
        meta_ [charset_ "utf-8"]
        link_ [rel_ "stylesheet", href_ "https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/css/bootstrap.min.css"]
    
    body_ [class_ "bg-light"] $ do
        nav_ [class_ "navbar navbar-expand-lg navbar-dark bg-primary"] $ do
            div_ [class_ "container"] $ do
                a_ [class_ "navbar-brand", href_ "/"] "🎮 Games Backlog"
        
        div_ [class_ "container mt-5"] $ do
            h1_ [class_ "mb-4"] "Confirmar Jogo"
            
            div_ [class_ "row"] $ do
                -- Coluna da capa
                div_ [class_ "col-md-4"] $ do
                    case maybeCover of
                        Just coverUrl -> 
                            img_ [src_ coverUrl, class_ "img-fluid rounded shadow", alt_ "Capa do jogo"]
                        Nothing -> 
                            div_ [class_ "bg-secondary text-white p-5 text-center rounded"] "Sem capa disponível"
                
                -- Coluna das informações
                div_ [class_ "col-md-8"] $ do
                    div_ [class_ "card p-4 shadow-sm"] $ do
                        h3_ [class_ "mb-3"] $ toHtml name
                        p_ [class_ "mb-2"] $ strong_ "Nota: " <> toHtml score
                        p_ [class_ "mb-4"] $ strong_ "Plataforma: " <> toHtml platform
                        
                        -- Formulário para confirmar
                        form_ [method_ "post", action_ "/confirm", class_ "d-flex gap-2"] $ do
                            input_ [type_ "hidden", name_ "name", value_ name]
                            input_ [type_ "hidden", name_ "score", value_ score]
                            input_ [type_ "hidden", name_ "platform", value_ platform]
                            button_ [type_ "submit", class_ "btn btn-success"] "Confirmar e Salvar"
                            a_ [href_ "/add", class_ "btn btn-secondary"] "Voltar"