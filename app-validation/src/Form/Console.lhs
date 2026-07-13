A camada de formulário de console
=================================

Enquanto `Form.Validation` é puro, este módulo cuida da interação com o
usuário: pergunta cada campo no terminal, lê a resposta, faz o *parsing* e
aplica a regra de validação. O tipo central é `Form`, que usa IO para 
executar o formulário no console. 

> module Form.Console
>   ( Form(..)
>   , field
>   , runForm
>   , renderErrors
>   , printResult
>   ) where
>
> import System.IO (hFlush, stdout)
> import Form.Validation

O tipo formulário
-----------------

Um `Form a` é um programa de console que, ao ser executado, produz uma
`Validation Errors a`: ou o valor `a` montado a partir das respostas, ou a
lista de todos os erros encontrados. Ou seja, `Form` é `IO` "por fora" e
`Validation Errors` "por dentro": precisamente `Compose IO (Validation
Errors)`. Escrevemos as instâncias à mão para deixar isso explícito.

> newtype Form a = Form { unForm :: IO (Validation Errors a) }

O `Functor` mapeia através das duas camadas: um `fmap` para entrar no `IO` e
outro para entrar na `Validation`.

> instance Functor Form where
>   fmap f (Form io) = Form (fmap (fmap f) io)

O `Applicative` é o que torna a construção de formulários elegante. `pure`
injeta um valor puro nas duas camadas. Já `(<*>)` roda os dois efeitos de
`IO` em sequência (perguntando ambos os campos) e então combina os resultados
com o `(<*>)` de `Validation`, que **acumula os erros** dos dois campos. É a
composição de aplicativos em ação: como `IO` e `Validation` são aplicativos, a
composição também é, de graça.

> instance Applicative Form where
>   pure = Form . pure . pure
>   Form f <*> Form x = Form ((<*>) <$> f <*> x)

Perguntando um campo
--------------------

`field label parse rule` constrói o formulário de um único campo:

  * imprime o rótulo e lê uma linha do usuário;
  * usa `parse` para converter o texto no tipo desejado (por exemplo, texto em
    `Int`) — um `Left` aqui vira um erro de *parsing*;
  * em caso de parsing bem-sucedido, aplica `rule` ao valor e anexa o nome do
    campo a cada mensagem, transformando `[String]` em `Errors`.

1. Com base no apresentado, implemente a função:

> field :: String -> (String -> Either String a) -> Rule a -> Form a
> field label parse rule = Form $ do
>   putStr (label ++ ": ")
>   hFlush stdout
>   input <- getLine
>   case parse input of
>     Left err ->
>       return (Failure [Error label err])
>     Right value ->
>       return $
>         mapFailure (map (Error label)) (runRule rule value)

Executando e relatando
----------------------

Executar um formulário é apenas revelar o `IO` interno.

> runForm :: Form a -> IO (Validation Errors a)
> runForm = unForm

2. Implemente uma função para realizar a renderização simples 
   da lista de erros: uma linha por erro, no formato `[campo] mensagem`.

> renderErrors :: Errors -> String
> renderErrors =
>   unlines . map (\e ->
>     "[" ++ errorField e ++ "] " ++ errorMessage e)

3. Implemente uma função para exibir o resultado da validação de um formulário: 
caso seja sucesso, mostra o valor; no caso de falha, lista todos os 
erros acumulados.

> printResult :: Show a => Validation Errors a -> IO ()
> printResult (Success x) =
>   print x
>
> printResult (Failure es) =
>   putStr (renderErrors es)
