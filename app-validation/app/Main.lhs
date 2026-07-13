Exemplo: validação de dados de uma pessoa
=========================================

Este executável demonstra a biblioteca montando um formulário para cadastrar
uma pessoa. Uma pessoa tem **nome** e **idade**, e as regras são:

  * o nome não pode ser vazio;
  * a idade deve ser maior ou igual a 0.

Todas as mensagens são customizadas na própria definição do formulário.

> module Main (main) where
>
> import Form.Validation (notEmpty, nonNegative)
> import Form.Console    (Form, field, runForm, printResult)

O tipo que queremos construir a partir das respostas do usuário.

> data Person = Person
>   { name :: String
>   , age  :: Int
>   } deriving Show

Um *parser* de inteiros com mensagem de erro amigável, usado no campo idade.
`reads` consome o texto; só aceitamos quando ele é integralmente um número.

> parseInt :: String -> Either String Int
> parseInt s = case reads s of
>   [(n, "")] -> Right n
>   _         -> Left "idade deve ser um número inteiro"

O formulário da pessoa. Repare como a estrutura aplicativa deixa a definição
declarativa: aplicamos o construtor `Person` aos campos com `(<$>)` e `(<*>)`,
e cada campo recebe seu rótulo, seu *parser* e sua regra com mensagem
customizada. Como o formulário é um aplicativo acumulador, se ambos os campos
forem inválidos, os dois erros são reportados de uma só vez.

> personForm :: Form Person
> personForm =
>   Person
>     <$> field "nome"  Right     (notEmpty    "nome não pode ser vazio")
>     <*> field "idade" parseInt  (nonNegative "idade deve ser maior ou igual a 0")

O programa principal: roda o formulário e imprime o resultado.

> main :: IO ()
> main = do
>   result <- runForm personForm
>   printResult result
