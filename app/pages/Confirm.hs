{-# LANGUAGE OverloadedStrings #-}

module Pages.Confirm where

import Lucid
import Data.Text (Text)
import qualified Data.Text as T

confirmPage :: Text -> Text -> Text -> Maybe Text -> Html ()
confirmPage name score platform maybeCover = html_ $ do
    head_ $ do
        title_ "Confirmar Jogo - Games Backlog"
        meta_ [charset_ "utf-8"]
        link_ [rel_ "stylesheet", href_ "https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/css/bootstrap.min.css"]
        style_ customStyle
    body_ [] $ do
        nav_ [class_ "navbar navbar-expand-lg navbar-dark bg-primary"] $
            div_ [class_ "container"] $
                a_ [class_ "navbar-brand", href_ "/"] "🎮 Games Backlog"
        
        div_ [class_ "container mt-5"] $ do
            h1_ [class_ "mb-4 text-center text-dark"] "Confirmar Jogo"
            
            div_ [class_ "row justify-content-center"] $
                div_ [class_ "col-12 col-sm-10 col-md-8 col-lg-6"] $
                    div_ [class_ "game-card mb-4 position-relative", style_ cardStyle] $ do
                        div_ [class_ "game-img-col"] $
                            case maybeCover of
                                Just coverUrl -> img_ [src_ coverUrl, class_ "game-cover", alt_ "Capa do jogo"]
                                Nothing -> div_ [class_ "bg-secondary text-white text-center rounded w-100", style_ "height:140px; display:flex; align-items:center; justify-content:center;"] "Sem capa disponível"
                        div_ [class_ "game-col flex-grow-1"] $ do
                            div_ [class_ "game-title mb-1"] $ toHtml name
                            div_ [class_ "game-info mb-1"] $ strong_ "Nota: " <> toHtml score
                            div_ [class_ "game-info mb-1"] $ strong_ "Plataforma: " <> toHtml platform
            
            div_ [class_ "row justify-content-center mt-4"] $
                div_ [class_ "col-lg-8 text-center"] $ do
                    form_ [method_ "post", action_ "/confirm", class_ "d-inline-block"] $ do
                        input_ [type_ "hidden", name_ "name", value_ name]
                        input_ [type_ "hidden", name_ "score", value_ score]
                        input_ [type_ "hidden", name_ "platform", value_ platform]
                        button_ [type_ "submit", class_ "btn btn-success btn-lg px-5"] "Confirmar e Salvar"
                    a_ [href_ "/add", class_ "btn btn-secondary btn-lg ms-3"] "Voltar"
  where
    (cardBg, cardBorder) = case platform of
        "PlayStation" -> ("#e3ecfa", "#0050d9")
        "Nintendo"    -> ("#ffeaea", "#e60012")
        "PC"          -> ("#e6e6e6ff", "#303030ff")
        "Xbox"        -> ("#eafaf1", "#107c10")
        _             -> ("#fff", "#bbb")
    
    cardStyle = T.concat
        [ "background:", cardBg, ";"
        , "border-bottom: 14px solid ", cardBorder, ";"
        , "margin-bottom: 12px;"
        ]

customStyle :: Text
customStyle = T.concat
    [ "body { background: #f8f9fa; }"
    , ".game-card { border-radius: 1.5rem 1.5rem 2.5rem 2.5rem; box-shadow: 0 4px 16px 0 rgba(31,38,135,0.10); border: 1px solid #e0e0e0; color: #222; overflow: hidden; position: relative; padding: 0; background: #fff; transition: box-shadow 0.2s; height: 140px; display: flex; }"
    , ".game-card:hover { box-shadow: 0 8px 32px 0 rgba(31,38,135,0.18); }"
    , ".game-cover { border-radius: 1.5rem 0 0 1.5rem; box-shadow: 0 2px 8px 0 rgba(0,0,0,0.06); background: #222; height: 140px; width: 120px; object-fit: cover; display: block; }"
    , ".game-title { font-size: 1.3rem; font-weight: bold; letter-spacing: 0.5px; }"
    , ".game-info { font-size: 1rem; }"
    , ".game-col { padding: 0.8rem 1rem !important; display: flex; flex-direction: column; justify-content: center; height: 140px; }"
    , ".game-img-col { padding: 0 !important; display: flex; align-items: center; justify-content: flex-start; background: #f0f0f0; width: 120px; height: 140px; }"
    ]