module Main where

import Huffman
import qualified Data.Map as Map
import System.FilePath
import System.Directory (createDirectoryIfMissing)

main :: IO ()
main = do
    putStrLn "Digite o nome do arquivo:"

    arquivo <- getLine

    texto <- readFile arquivo

    let arvore = buildTree (makeLeaves (frequency texto))
    let tabela = codes arvore
    let comprimido = encode tabela texto

    createDirectoryIfMissing True "comprimido"

    let nomeArquivo = takeBaseName arquivo
    let novoArquivo = "comprimido/" ++ nomeArquivo ++ "-comprimido.txt"
 
    writeFile novoArquivo comprimido

    putStrLn "\nTabela de códigos:"
    mapM_ print (Map.toList tabela)

    putStrLn "\nTexto original:"
    putStrLn texto

    putStrLn "\nTexto comprimido:"
    putStrLn comprimido

    putStrLn "\nTexto decodificado:"
    putStrLn (decode arvore comprimido)

    putStrLn "\nArquivo salvo em comprimido.txt"