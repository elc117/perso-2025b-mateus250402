# Trabalho backend web

**Identificação:** Mateus Cardoso Oliveira, Sistemas de Informação - UFSM

**Tema/objetivo:** Esse trabalho tem como objetivo criar um sistema web para um backlog de jogos, o foco do trabalho está no desenvolvimento do backend utilizando a linguagem de programação funcional Haskell.

**Processo de desenvolvimento:**

Esse projeto "nasce" há 2 anos, quando tive meus primeiros contatos com programção, tentei construir um aplicativo mobile com esse mesmo objetivo, porém por falta de conhecimento na época ficou aquém do desejável, agora trago de volta essa ideia com uma maior capacidade de criação e compreensão.
Iniciei o desenvolvimento dele com um "esquema pronto" na cabeça, usuários, autenticação, sessão, banco de dados, a ideia dos cartões coloridos e seu desing em si. 

Primeiramente pesquisei sobre algumas biblitecas gráficas para utilizar, encontrei no Lucid o que precisava, algo que lembra HTML (que já tive contato) e que pudesse utilizar o framework Bootstrap para não dedicar muito tempo ao frontend, visto o objetivo principal do trabalho e minha vontade pessoal de seguir nessa linha. Visto o tamanho do projeto, o tempo disponível e meu desejo do que eu gostaria de fazer, optei pelo uso de IA para gerar código para o frontend, claro, fiz as adaptações necessárias para a perfeita exibição de como imaginei e também para manter um estilo padrão em todo site. Pude aprender sobre a sintaxe do Lucid, como o mesmo aplica os atributos nas tag HTML e percebi por ser um código em Haskell uma facilidade em alternar entre os estilos dos cards - ponto que não consegui superar na primeira concepção dessa ideia, há 2 anos - pois consegui com facilidade passar uma variável "gPlatform" e partindo dela decidir o estilio aplicado.
```haskell
let (cardBg, cardBorder) = case gPlatform of
    "PlayStation" -> ("#e3ecfa", "#0050d9")
    "Nintendo"    -> ("#ffeaea", "#e60012")
    "PC"          -> ("#e6e6e6ff", "#303030ff")
    "Xbox"        -> ("#eafaf1", "#107c10")
```
O próximo passo, visto minha experência prévia com web, foi a criação do banco de dados com sqlite com as tabela user e games, além disso as funções de registro e login. Nessa parte encontrei pouco dificuldade, mas lembro de que foi uma gigantesca barreira na primeira tentativa de desenvolver esse projeto por não estar habituado com tal linuagem. Derivando disso, comecei a esquematizar a ideia de sessão, para poder inserir o jogos com o id do usuário logado, além de não permitir acesso a certos mecanisos aos usuário não logados, como já tive experiência anteriormente em Python pensei que seria trivial, porém não encontrei no Scotty algo já pronto para isso, na verdade encontrei sim porém não era compatível com a minha versão, então pedi ajuda para um gerador de código sobre como fazer as funções necessárias, resultando nas que apresentarei junto do meu entendimento sobre:
```haskell
sessionInsert :: String -> String -> ActionM ()
sessionInsert key value = do
    let cookie = defaultSetCookie 
            { setCookieName = TE.encodeUtf8 $ T.pack key 
            , setCookieValue = TE.encodeUtf8 $ T.pack value
            , setCookiePath = Just "/" 
            }
    setHeader "Set-Cookie" $ TL.fromStrict $ TE.decodeUtf8 $ BS.toStrict $ toLazyByteString $ renderSetCookie cookie 
```
Essa função recebe o nome de um cookie (que funciona como um aramazenamento do navegador) e o valor a inserir, o nome é dado pelo argumento key e o valor por value, os mesmo são formatodos para o formato esperado por suas respectivas funções, ```setCookiePath = Just "/"``` habilita o cookie para toda a estrutura do site. Por fim, o cookie é inserido no header do site para armazenar.

```haskell
sessionLookup :: String -> ActionM (Maybe String)
sessionLookup key = do
    req <- request
    let headers = requestHeaders req -
    case lookup "Cookie" headers of 
        Nothing -> return Nothing
        Just cookieHeader -> do
            let cookies = parseCookies cookieHeader
                keyBS = TE.encodeUtf8 $ T.pack key 
            case lookup keyBS cookies of
                Nothing -> return Nothing
                Just valueBS -> return $ Just $ T.unpack $ TE.decodeUtf8 valueBS
```
Já essa função serve para procurar no header um cookie especificaod pelo valor key e retornar seu valor, o que foi utilizado para obter o id do usuário e verificar se estava logado.

```haskell
requireAuth :: ActionM () -> ActionM ()
requireAuth action = do
    userId <- sessionLookup "user_id"
    case userId of
        Just "" -> redirect "/login"
        Just _ -> action
        Nothing -> redirect "/login"
```
Essa função foi utilizada para validar se o usuário estava logado, caso não estivesse redireciona para a página de login, a função verifica se o cookie de user_id apresenta um valor ou não. A verificação de "" dá-se devido ao ```postLogout``` em Handles.hs que ao fazer o logout insere o valor "" no cookie user_id. Aqui vale ressaltar que enfrentei um erro no momento do login, pois pensava não ser necessário verificar "" na função, a solução foi simples, apenas adicionei essa verificação.

Agora indo para as funções que interagem com o banco de dados a maior parte delas foi de fácil desenvolvimento, pedi para um assistente de IA se havia algum tipo de convenção ou recomendação na construção desse tipo de função, o mesmo informou sobre alguns tipos de retorno até então desconhecidos como o uso de `Maybe`, `Either` e `Connection`. Pedindo explicações sobre entendi que o `Maybe` seria para indicar que pode ou não ter aquele retorno, no contexto notei na inserção da url da capa, pois ao pegar esse valor nas funções de integração com a API poderia retornar ou não um Text na capa, no caso do jogo não possuir capa. `Either` aprendi que indica quando há mais de um retorno para a mesma função, por exemplo em `insertUser :: Text -> Text -> IO (Either String Int)` que pode retornar uma mensgaem de erro (`String`) ou como sucesso o id do user (`Int`). Já para `Connection` entendi ser para representar uma conexão, visto que o mesmo é o retorno de `connectDB :: IO Connection`.

O próximo passo foi um desenvolvimento conjunto entre alguns dos Handles da aplicação e as funções da API Igdb da Twitch https://www.igdb.com/api

**Orientações para execução:**

Para rodar o software abra no navegador http://localhost:3000/ e digite no terminal:
```haskell
cabal build
cabal run
```

Para execução dos teste digite no terminal:
```haskell
cabal build --enable-test
cabal test
```

**Resultado final:** [Assita ao vídeo do resultado final](images/backlog.mp4)

- Referências e créditos (incluindo alguns prompts, se aplicável)