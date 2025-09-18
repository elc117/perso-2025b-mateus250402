# Trabalho Backend Web

**Identificação:** Mateus Cardoso Oliveira, Sistemas de Informação - UFSM

**Tema/Objetivo:**  
Esse trabalho tem como objetivo criar um sistema web para um backlog de jogos, com foco no desenvolvimento do backend utilizando a linguagem de programação funcional Haskell.

## Processo de Desenvolvimento

Este projeto "nasceu" há 2 anos, quando tive meus primeiros contatos com programação. Na época, tentei construir um aplicativo mobile com o mesmo objetivo, mas por falta de conhecimento o resultado ficou aquém do desejável. Agora, retomo essa ideia com maior capacidade de criação e compreensão.

Iniciei o desenvolvimento com um "esquema pronto" em mente: usuários, autenticação, sessões, banco de dados, a ideia dos cartões coloridos e seu design em si.

Primeiramente, pesquisei algumas bibliotecas gráficas para utilizar. Encontrei no **Lucid** o que precisava: algo que lembra HTML (com o qual já tinha contato) e que me permitisse usar o **framework Bootstrap**, economizando tempo no frontend, visto que o foco principal do trabalho é o backend.

Considerando o tamanho do projeto, o tempo disponível e minhas preferências, optei por usar IA para gerar código para o frontend. Claro, fiz adaptações necessárias para que a exibição ficasse como imaginei e para manter um estilo padrão em todo o site.  

Durante o processo, aprendi sobre a sintaxe do Lucid, como ele aplica atributos nas tags HTML e percebi que, por se tratar de código Haskell, é fácil alternar estilos dos cards. Um ponto que não consegui superar na primeira concepção desse projeto, há 2 anos, agora foi resolvido: consegui, com facilidade, passar uma variável `gPlatform` e, a partir dela, definir o estilo aplicado.

```haskell
let (cardBg, cardBorder) = case gPlatform of
    "PlayStation" -> ("#e3ecfa", "#0050d9")
    "Nintendo"    -> ("#ffeaea", "#e60012")
    "PC"          -> ("#e6e6e6ff", "#303030ff")
    "Xbox"        -> ("#eafaf1", "#107c10")
```
O próximo passo, considerando minha experiência prévia com desenvolvimento web, foi a criação do banco de dados usando **SQLite**, com as tabelas `user` e `games`, além das funções de registro e login. Nessa etapa, encontrei pouca dificuldade, mas lembro que na primeira tentativa de desenvolver este projeto foi uma grande barreira por não estar habituado à linguagem.

A partir disso, comecei a esquematizar a ideia de **sessão**, para poder associar os jogos ao `id` do usuário logado e, ao mesmo tempo, impedir que usuários não logados acessassem certos mecanismos. 

Com experiência prévia em Python, achei que seria trivial implementar, mas percebi que no **Scotty** não havia algo pronto para isso compatível com a versão que utilizo. Então, recorri a um gerador de código para obter as funções necessárias, resultando nas implementações de funções para cookies, assunto que eu não conhecia, então tive a oportunidade de aprender sobre como o programa "armazena" na memório do navegador certos valores.

Para os testes utilizei o hSpec, percebi que possui um sintaxe bem intuitiva, o teste funciona executanod uma função e comparando o resultado da mesma a um resultado fixo que seria o esperado, nessa parte enfrentei alguns problemas com os testes de banco de dados, por vezes errei o valor de retorno e acabava não chegando na limpeza do teste, ficando com usuários de teste e jogos de teste inseridos no banco de dados.

Por fim, enfrentei diversos problemas com a tipagem esperado por funções de bibliotecas, ponto no qual utilizei muito de Intelgência Artificial para compreender de forma rápida qual seria o tipo esperado por certa função. O desenvolvimento contou com constante refatoração e revisão do código, o uso de geradores de código e IA para apenas explicar determinadas dúvidas possibilitou um desenvolvimento mais rápido, claro, com constante verficação, pois a Intelgência Artificial é uma ótima ferramente, e não algo mágico que soluciona qualquer problema.

# Orientações para Execução

## Rodando o Software

Para executar o software, abra o navegador em:  
[http://localhost:3000/](http://localhost:3000/)

No terminal, execute os comandos:

```haskell
cabal build
cabal run
```

## Executando os testes

No terminalm digite os seguintes comandos para rodar os testes:
```haskell
cabal build --enable-test
cabal test
```

# Resultado Final

[Assista ao vídeo do resultado final](images/backlog.mp4)


# Referências e Créditos

- **Scotty**: [https://github.com/scotty-web/scotty?tab=readme-ov-file](https://github.com/scotty-web/scotty?tab=readme-ov-file)  
- **Lucid**: [https://hackage.haskell.org/package/lucid](https://hackage.haskell.org/package/lucid)  
- **Código JuvEnart**: funções `autenticar`, `insert`, `delete` de minha autoria.  
- **Gabriel Vargas Saueressig**

---

# Prompts e Perguntas do Projeto

1. **Bootstrap das páginas**  
   Foi solicitado que a IA gerasse o bootstrap das páginas, visto que o foco principal do trabalho é o backend web. Algumas adaptações foram feitas para o contexto do projeto.

2. **Conexão com o banco de dados**  
   Preciso criar uma função para conectar ao banco de dados.

3. **Inserir valores em uma tabela**  
   Solicitei um exemplo de como inserir valores em uma tabela.

4. **Hash de senha**  
   Como gerar um hash para senhas de usuários.

5. **Estrutura `case of`**  
   Perguntei como funciona o `case of` em Haskell. É semelhante a um `switch case`.

6. **Anotação de tipo IO**  
   Necessidade de usar a anotação de tipo: `IO (Either SomeException (Maybe Int))`.

7. **Sessões com Scotty**  
   Tentei usar Scotty Session, mas parece incompatível com a versão que uso. Perguntei como implementar funções de sessão de outra forma.

8. **Requisição à API IGDB**  
   Como realizar uma requisição HTTP para obter dados da API IGDB.

9. **Obter informações de uma tabela**  
   Como pegar dados de uma tabela para uso posterior no código.

10. **Filtragem de dados**  
    Como aplicar filtros para exibir informações específicas.
