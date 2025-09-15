{-# LANGUAGE OverloadedStrings #-}

module Pages.Backlog where

import Lucid
import Models.Games (Game(..))
import Data.Text (Text)
import qualified Data.Text as T

backlogPage :: [Game] -> Html ()
backlogPage games = html_ $ do
    head_ $ do
        title_ "Meu Backlog - Games Backlog"
        meta_ [charset_ "utf-8"]
        link_ [rel_ "stylesheet", href_ "https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/css/bootstrap.min.css"]
        style_ customStyle
    body_ [] $ do
        nav_ [class_ "navbar navbar-expand-lg navbar-dark bg-primary"] $
            div_ [class_ "container"] $
                a_ [class_ "navbar-brand", href_ "/"] "🎮 Games Backlog"
        div_ [class_ "container mt-5"] $ do
            h1_ [class_ "mb-4 text-center text-dark fw-bold"] "Meu Backlog"
            div_ [class_ "mb-4 text-center"] $ do
                a_ [href_ "/add", class_ "btn btn-success me-2"] "Adicionar Jogo"
                a_ [href_ "/", class_ "btn btn-outline-primary"] "Home"
            h2_ [class_ "mt-4 mb-3 text-secondary"] "Jogos Salvos"
            if null games
                then p_ [class_ "text-center text-muted fs-4"] "Nenhum jogo salvo ainda."
                else div_ [class_ "d-flex flex-row flex-wrap align-items-start w-100", style_ ""] $
                        mapM_ backlogCard games

backlogCard :: Game -> Html ()
backlogCard (Game title score platform cover_url) =
    let (cardBg, cardBorder) = case platform of
            "PlayStation" -> ("#e3ecfa", "#0050d9")
            "Nintendo"    -> ("#ffeaea", "#e60012")
            "PC"          -> ("#f5f5f5", "#222")
            "Xbox"        -> ("#eafaf1", "#107c10")
            _             -> ("#fff", "#bbb")
        cardStyle = T.concat
            [ "background:", cardBg, ";"
            , "border-bottom: 14px solid ", cardBorder, ";"
            , "margin-bottom: 12px;"
            ]
    in div_ [style_ "flex: 1 1 31%; max-width: 31%; min-width: 300px; margin-right: 2%; margin-left: 0;"] $ 
        div_ [class_ "game-card mb-2 position-relative", style_ cardStyle] $ do
            div_ [class_ "game-img-col"] $
                case cover_url of
                    Just url -> img_ [src_ url, class_ "game-cover", alt_ "Capa do jogo"]
                    Nothing  -> div_ [class_ "bg-secondary text-white text-center rounded w-100", style_ "height:140px; display:flex; align-items:center; justify-content:center;"] "Sem capa disponível"
            div_ [class_ "game-col flex-grow-1"] $ do
                div_ [class_ "game-title mb-1"] $ toHtml title
                div_ [class_ "game-info mb-1"] $ strong_ "Nota: " <> toHtml (show score)
                div_ [class_ "game-info mb-1"] $ strong_ "Plataforma: " <> toHtml platform

customStyle :: Text
customStyle = T.concat
    [ "body { background: #f8f9fa; }"
    , ".game-card { border-radius: 1.5rem 1.5rem 2.5rem 2.5rem; box-shadow: 0 4px 16px 0 rgba(31,38,135,0.10); border: 1px solid #e0e0e0; color: #222; overflow: hidden; position: relative; padding: 0; background: #fff; transition: box-shadow 0.2s; height: 140px; display: flex; }"
    , ".game-card:hover { box-shadow: 0 8px 32px 0 rgba(31,38,135,0.18); }"
    , ".game-card-bottom { height: 14px; width: 100%; position: absolute; left: 0; bottom: 0; border-radius: 0 0 2.5rem 2.5rem; }"
    , ".game-cover { border-radius: 1.5rem 0 0 1.5rem; box-shadow: 0 2px 8px 0 rgba(0,0,0,0.06); background: #222; height: 140px; width: 120px; object-fit: cover; display: block; }"
    , ".game-title { font-size: 1.3rem; font-weight: bold; letter-spacing: 0.5px; }"
    , ".game-info { font-size: 1rem; }"
    , ".game-col { padding: 0.8rem 1rem !important; display: flex; flex-direction: column; justify-content: center; height: 140px; }"
    , ".game-img-col { padding: 0 !important; display: flex; align-items: center; justify-content: flex-start; background: #f0f0f0; width: 120px; height: 140px; }"
    ]