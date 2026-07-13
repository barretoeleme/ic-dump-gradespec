Validação usando applicative functors 
=====================================

Nesse módulo vamos modelar uma abordagem composicional 
para validação utilizando *applicative functors*. A 
ideia é usar essa classe de tipos para permitir que 
diferentes critérios de validação sejam compostos e, 
partir destes, podemos criar formulários de maneira 
simples.

Funções a serem implementadas são marcadas por 
*undefined*. 

> module Form.Validation
>   ( -- * Erros
>     Error(..)
>   , Errors
>     -- * O aplicativo de validação
>   , Validation(..)
>   , mapFailure
>   , validationToEither
>     -- * Regras de campo
>   , Rule(..)
>   , satisfies
>   , notEmpty
>   , nonNegative
>   ) where

Erros
-----

Um erro guarda o nome do campo que falhou e uma mensagem legível. Manter o
campo junto da mensagem permite, mais tarde, imprimir mensagens claras como
`[idade] idade deve ser maior ou igual a 0`.

> data Error = Error
>   { errorField :: String  -- nome do campo que falhou
>   , errorMessage :: String  -- mensagem legível para o usuário
>   } deriving (Eq, Show)

Como um formulário pode acumular vários erros, trabalhamos com listas de erro.

> type Errors = [Error]

O tipo para validadores
-------------------------

`Validation e a` representa o resultado de validar algo: ou falhou, carregando
os erros acumulados `e`, ou teve sucesso, carregando o valor `a`. É como
`Either e a`, mas com um `Applicative` fundamentalmente diferente.

> data Validation e a
>   = Failure e  -- validação falhou, com os erros acumulados
>   | Success a  -- validação bem-sucedida, com o valor produzido
>   deriving (Eq, Show)

1. Implemente uma instância de `Functor` para o tipo Validation. 
A ideia é que o functor deve ser definido sobre `Validation e`. 
Erros devem ser mantidos intactos e o sucesso deve ter seu valor 
modificado pela função fornecida como argumento para `fmap`.

O `Functor` é o esperado: transformamos o valor em caso de sucesso e
propagamos o erro intacto em caso de falha.

> instance Functor (Validation e) where
>   fmap _ (Failure e) = Failure e
>   fmap f (Success x) = Success (f x)

2. Combinamos validadores utilizando a classe applicative. A função 
   *pure* deve criar um validador que nunca falha. Para combinar duas validações com `<*>`,
   quando **ambas** falham nós juntamos os erros com `(<>)`. É por isso que o
   tipo dos erros precisa ser um `Semigroup`: é ele quem sabe combinar dois
   conjuntos de erros num só (para `Errors = [Error]`, `(<>)` é a concatenação
   de listas).

> instance Semigroup e => Applicative (Validation e) where
>   pure = Success
>
>   Success f <*> Success x = Success (f x)
>   Failure e <*> Failure e' = Failure (e <> e')
>   Failure e <*> _ = Failure e
>   _ <*> Failure e = Failure e

3. Implemente uma versão de map que opera apenas sobre os erros: 

> mapFailure :: (e -> e') -> Validation e a -> Validation e' a
> mapFailure f (Failure e) = Failure (f e)
> mapFailure _ (Success x) = Success x

4. Apresente uma função para converter um valor de tipo `Validation`
   em um equivalente do tipo `Either`.

> validationToEither :: Validation e a -> Either e a
> validationToEither (Failure e) = Left e
> validationToEither (Success x) = Right x

Regras de validação
-------------------

Uma `Rule a` é uma regra de validação sobre um valor do tipo `a`. Ela recebe
um valor e devolve uma `Validation`: ou o próprio valor (sucesso), ou uma
lista de **mensagens** de erro. Repare que a regra produz apenas mensagens
(`[String]`), não `Error` completos: ela é deliberadamente **agnóstica ao
nome do campo**, para poder ser reutilizada em qualquer campo. É a camada de
formulário que, conhecendo o nome do campo, transforma mensagem em `Error`.

> newtype Rule a = Rule { runRule :: a -> Validation [String] a }

5. Implemente a composição de regras.

Regras compõem com `(<>)`: aplicar `r1 <> r2` a um valor roda as duas regras e
**acumula** as mensagens de ambas.

> instance Semigroup (Rule a) where
>   Rule f <> Rule g =
>     Rule (\x -> pure x <* f x <* g x)

6. Implemente o combinador `satisfies p msg` produz a regra 
"o valor deve satisfazer o predicado `p`; se não satisfizer, reporte `msg`". 

> satisfies :: (a -> Bool) -> String -> Rule a
> satisfies p msg =
>   Rule (\x ->
>     if p x
>       then Success x
>       else Failure [msg])

A partir do combinador base derivamos regras prontas. Note que cada uma recebe
a mensagem a exibir, cumprindo o requisito de mensagens customizáveis.

`notEmpty` exige que uma string não seja vazia:

> notEmpty :: String -> Rule String
> notEmpty = satisfies (not . null)

`nonNegative` exige que um número seja maior ou igual a zero:

> nonNegative :: (Ord a, Num a) => String -> Rule a
> nonNegative = satisfies (>= 0)
