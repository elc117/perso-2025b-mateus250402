{-# LANGUAGE OverloadedStrings #-}

module Pages.Selection where

import Lucid
import Data.Text (Text)
import qualified Data.Text as T
import Api.Igdb (GameResult(..))

gameSelectionPage :: Text -> Text -> Text -> [GameResult] -> Html ()
gameSelectionPage originalName score originalPlatform gameResults = html_ $ do
    head_ $ do
        title_ "Selecionar Jogo - Games Backlog"
        meta_ [charset_ "utf-8"]
        link_ [rel_ "stylesheet", href_ "https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/css/bootstrap.min.css"]
        style_ customStyle
    
    body_ [] $ do
        nav_ [class_ "navbar navbar-expand-lg navbar-dark bg-primary"] $
            div_ [class_ "container"] $
                a_ [class_ "navbar-brand", href_ "/"] "🎮 Games Backlog"
        
        div_ [class_ "container mt-5"] $ do
            h1_ [class_ "mb-4 text-center text-dark"] "Selecionar o jogo correto"
            p_ [class_ "text-center text-muted mb-4"] $ "Encontramos " <> toHtml (show $ length gameResults) <> " jogos com o nome \"" <> toHtml originalName <> "\". Selecione o correto:"
            
            div_ [class_ "row"] $
                mapM_ (gameSelectionCard score originalPlatform) gameResults
            
            div_ [class_ "text-center mt-4"] $
                a_ [href_ "/add", class_ "btn btn-secondary"] "Voltar"

gameSelectionCard :: Text -> Text -> GameResult -> Html ()
gameSelectionCard score originalPlatform (GameResult name _ year coverUrl) = 
    div_ [class_ "col-md-6 col-lg-4 mb-4"] $
        div_ [class_ "card h-100 game-selection-card"] $ do
            case coverUrl of
                Just url -> img_ [src_ url, class_ "card-img-top game-selection-cover", alt_ "Capa do jogo"]
                Nothing -> div_ [class_ "card-img-top bg-secondary text-white d-flex align-items-center justify-content-center", style_ "height: 200px;"] "Sem capa"
            
            div_ [class_ "card-body d-flex flex-column"] $ do
                h5_ [class_ "card-title"] $ toHtml name
                p_ [class_ "card-text flex-grow-1"] $ do
                    case year of
                        Just y -> do
                            strong_ "Ano: "
                            toHtml $ show y
                        Nothing -> "Ano não informado"
                
                form_ [method_ "get", action_ "/confirm", class_ "mt-auto"] $ do
                    input_ [type_ "hidden", name_ "name", value_ name]
                    input_ [type_ "hidden", name_ "score", value_ score]
                    -- Mantém a plataforma original selecionada no formulário /add
                    input_ [type_ "hidden", name_ "platform", value_ originalPlatform]
                    case coverUrl of
                        Just url -> input_ [type_ "hidden", name_ "cover_url", value_ url]
                        Nothing -> input_ [type_ "hidden", name_ "cover_url", value_ ""]
                    button_ [type_ "submit", class_ "btn btn-primary w-100"] "Selecionar este jogo"

customStyle :: Text
customStyle = T.concat
    [ "body { background: #f8f9fa; }"
    , ".game-selection-card { transition: transform 0.2s, box-shadow 0.2s; }"
    , ".game-selection-card:hover { transform: translateY(-5px); box-shadow: 0 8px 16px rgba(0,0,0,0.1); }"
    , ".game-selection-cover { height: 200px; object-fit: cover; }"
    ]