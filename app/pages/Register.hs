{-# LANGUAGE OverloadedStrings #-}

module Pages.Register where

import Lucid

registerPage :: Html ()
registerPage = html_ [lang_ "pt-br"] $ do
    
    head_ $ do
        title_ "Registro - Games Backlog"
        meta_ [charset_ "utf-8"]
        meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
        link_ [href_ "https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/css/bootstrap.min.css", rel_ "stylesheet"]  -- ✅ ADICIONAR ESTA LINHA
    
    body_ $ do
        nav_ [class_ "navbar navbar-expand-lg navbar-dark bg-primary"] $ do
            div_ [class_ "container"] $ do
                a_ [class_ "navbar-brand", href_ "/"] "🎮 Games Backlog"
                div_ [class_ "navbar-nav ms-auto"] $ do
                    a_ [class_ "nav-link", href_ "/login"] "Login"
                    a_ [class_ "nav-link", href_ "/"] "Início"

        div_ [class_ "container"] $ do
            div_ [class_ "row justify-content-center"] $ do
                div_ [class_ "col-md-6 col-lg-4"] $ do
                    div_ [class_ "card mt-5 shadow bg-light"] $ do
                        div_ [class_ "card-header text-center bg-primary text-white"] $ do
                            h4_ [class_ "mb-0"] "📝 Criar Conta"
                        
                        div_ [class_ "card-body p-4"] $ do
                            form_ [method_ "post", action_ "/register"] $ do  -- ✅ VERIFICAR ESTA LINHA
                                div_ [class_ "mb-3"] $ do
                                    label_ [for_ "email", class_ "form-label"] "E-mail"
                                    input_ [type_ "email", class_ "form-control", id_ "email", 
                                           name_ "email", required_ "", placeholder_ "Digite seu e-mail"]  -- ✅ VERIFICAR name="email"
                                
                                div_ [class_ "mb-3"] $ do
                                    label_ [for_ "password", class_ "form-label"] "Senha"
                                    input_ [type_ "password", class_ "form-control", id_ "password", 
                                           name_ "password", required_ "", placeholder_ "Crie uma senha segura"]  -- ✅ VERIFICAR name="password"
                                
                                div_ [class_ "d-grid"] $ do
                                    button_ [type_ "submit", class_ "btn btn-success"] "Registrar"  -- ✅ VERIFICAR type="submit"
                        
                        div_ [class_ "card-footer text-center bg-primary"] $ do
                            span_ [class_ "text-light"] "Já tem conta? "
                            a_ [href_ "/login", class_ "text-white text-decoration-none"] "Faça login"
                            br_ []
                            a_ [href_ "/", class_ "text-muted text-decoration-none"] "← Voltar ao início"
        
        script_ [src_ "https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/js/bootstrap.bundle.min.js"] ("" :: Html ())