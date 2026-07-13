Testes puros da validação
=========================

Aqui exercitamos o núcleo aplicativo sem nenhum I/O de console. Reconstruímos
a validação de `Person` diretamente sobre `Validation`, com o nome de cada
campo fixo, e verificamos os quatro casos que importam — em especial o caso
que **prova a acumulação applicative**: nome vazio *e* idade negativa devem
produzir os dois erros de uma vez.

> module Main (main) where
>
> import Control.Monad (unless)
> import System.Exit   (exitFailure)
> import Form.Validation

O tipo de teste e a validação pura. Anexamos o nome do campo a cada mensagem
com `mapFailure`, exatamente como a camada de console faz, e combinamos os
campos com o `Applicative` de `Validation`.

> data Person = Person String Int deriving (Eq, Show)
>
> validatePerson :: String -> Int -> Validation Errors Person
> validatePerson nm ag =
>   Person
>     <$> tag "nome"  (runRule (notEmpty    "nome não pode ser vazio")        nm)
>     <*> tag "idade" (runRule (nonNegative "idade deve ser maior ou igual a 0") ag)
>   where tag f = mapFailure (map (Error f))

Um mini-arcabouço de testes: cada caso é um par (nome, resultado esperado ==
obtido).

> type Case = (String, Bool)
>
> messagesOf :: Validation Errors a -> [String]
> messagesOf (Failure es) = map errorMessage es
> messagesOf (Success _)  = []

Os casos:

> cases :: [Case]
> cases =
>   [ ( "pessoa válida é aceita"
>     , validatePerson "Ana" 30 == Success (Person "Ana" 30) )
>
>   , ( "nome vazio é rejeitado"
>     , messagesOf (validatePerson "" 30) == ["nome não pode ser vazio"] )
>
>   , ( "idade negativa é rejeitada"
>     , messagesOf (validatePerson "Ana" (-5))
>         == ["idade deve ser maior ou igual a 0"] )
>
>   , ( "erros acumulam: nome vazio E idade negativa"
>     , messagesOf (validatePerson "" (-5))
>         == [ "nome não pode ser vazio"
>            , "idade deve ser maior ou igual a 0" ] )
>   ]

O ponto de entrada: roda todos os casos, imprime o desfecho de cada um e
falha o processo (código de saída ≠ 0) se algum não passar.

> main :: IO ()
> main = do
>   mapM_ report cases
>   unless (all snd cases) exitFailure
>   where
>     report (nm, ok) = putStrLn ((if ok then "ok   - " else "FALHA - ") ++ nm)
