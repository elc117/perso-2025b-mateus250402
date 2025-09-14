{-# LANGUAGE OverloadedStrings #-}

module Pages.Confirm where

import Lucid
import Data.Text (Text)

confirmPage :: Text -> Text -> Text -> Maybe Text -> Html ()
confirmPage name score platform maybeCover = html_ $ do
    let (cardBg, cardBorder) = case platform of
            "PlayStation" -> ("#e3ecfa", "#0050d9")
            "Nintendo"    -> ("#ffeaea", "#e60012")
            "PC"          -> ("#f5f5f5", "#222")
            "Xbox"        -> ("#eafaf1", "#107c10")
            _             -> ("#181c22", "#fff")
        customStyle = mconcat
            [ "body { background: #fff; }"
            , ".game-card { background:", cardBg, "; border-radius: 1.5rem 1.5rem 2.5rem 2.5rem; box-shadow: 0 8px 32px 0 rgba(31,38,135,0.15); border: 1px solid #222; color: #222; overflow: hidden; position: relative; padding: 0.5rem 0.5rem 2.2rem 0.5rem; }"
            , ".game-card-bottom { height: 18px; width: 100%; background:", cardBorder, "; position: absolute; left: 0; bottom: 0; border-radius: 0 0 2.5rem 2.5rem; }"
            , ".game-cover { border-radius: 1rem; box-shadow: 0 4px 16px 0 rgba(0,0,0,0.08); background: #222; max-height: 140px; width: auto; object-fit: contain; }"
            , ".game-title { font-size: 1.5rem; font-weight: bold; letter-spacing: 1px; }"
            , ".game-info { font-size: 1rem; }"
            , ".game-col { padding: 0rem !important; }"
            ]
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
                div_ [class_ "col-12 col-sm-10 col-md-8 col-lg-5"] $
                    div_ [class_ "game-card mb-4 position-relative"] $ do
                        div_ [class_ "row g-0 align-items-center"] $ do
                            -- Imagem menor à esquerda
                            div_ [class_ "col-md-6 game-col text-center"] $
                                case maybeCover of
                                    Just coverUrl ->
                                        img_ [src_ coverUrl, class_ "game-cover", alt_ "Capa do jogo"]
                                    Nothing ->
                                        div_ [class_ "bg-secondary text-white p-3 text-center rounded"] "Sem capa disponível"
                            -- Informações à direita
                            div_ [class_ "col-md-6 game-col"] $ do
                                div_ [class_ "game-title mb-2"] $ toHtml name
                                div_ [class_ "game-info mb-1"] $ strong_ "Nota: " <> toHtml score
                                div_ [class_ "game-info mb-2"] $ strong_ "Plataforma: " <> toHtml platform
                        -- Borda inferior colorida
                        div_ [class_ "game-card-bottom"] ("" :: Html ())
            div_ [class_ "row justify-content-center"] $
                div_ [class_ "col-lg-8 text-center"] $ do
                    form_ [method_ "post", action_ "/confirm", class_ "d-inline-block"] $ do
                        input_ [type_ "hidden", name_ "name", value_ name]
                        input_ [type_ "hidden", name_ "score", value_ score]
                        input_ [type_ "hidden", name_ "platform", value_ platform]
                        button_ [type_ "submit", class_ "btn btn-success btn-lg px-5"] "Confirmar e Salvar"
                    a_ [href_ "/add", class_ "btn btn-secondary btn-lg ms-3"] "Voltar"