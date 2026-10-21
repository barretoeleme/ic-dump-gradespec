import System.Process
import System.Exit (ExitCode(..))
import System.Directory (listDirectory)
import Data.List (sort)

executarPython :: FilePath -> String -> IO (ExitCode, String, String)
executarPython arquivo entrada =
    readProcessWithExitCode "python3" [arquivo] entrada

executarGabarito :: FilePath -> String -> IO String
executarGabarito gabarito entrada = do
    (codigo, saida, erro) <- executarPython gabarito entrada

    case codigo of
        ExitSuccess -> return saida
        ExitFailure n -> do
            putStrLn ("Erro no gabarito: " ++ show n)
            putStrLn erro
            return ""

corrigirAluno :: FilePath -> String -> String -> IO Bool
corrigirAluno aluno entrada saidaEsperada = do
    (codigo, saida, erro) <- executarPython aluno entrada

    case codigo of
        ExitSuccess ->
            return (saida == saidaEsperada)

        ExitFailure n -> do
            putStrLn ("Erro ao executar " ++ aluno)
            putStrLn erro
            return False

corrigirTodosTestes :: FilePath -> [FilePath] -> IO ()
corrigirTodosTestes aluno testes = do
    putStrLn ("\nCorrigindo: " ++ aluno)
    corrigir aluno testes 1 0

corrigir :: FilePath -> [FilePath] -> Int -> Int -> IO ()
corrigir aluno [] numero acertos = do
    putStrLn ("Resultado: " ++ show acertos ++
              "/" ++ show (numero - 1))

corrigir aluno (teste:resto) numero acertos = do
    entrada <- readFile teste
    saidaEsperada <- executarGabarito "gabarito.py" entrada

    resultado <- corrigirAluno aluno entrada saidaEsperada

    if resultado
        then putStrLn ("Teste " ++ show numero ++ ": OK")
        else putStrLn ("Teste " ++ show numero ++ ": ERRO")

    let novosAcertos =
            if resultado then acertos + 1 else acertos

    corrigir aluno resto (numero + 1) novosAcertos

main :: IO ()
main = do
    arquivosTestes <- listDirectory "entradas"
    arquivosAlunos <- listDirectory "alunos"

    let testes = sort
            [ "entradas/" ++ nome
            | nome <- arquivosTestes
            , ".txt" `isSufixoDe` nome
            ]

    let alunos = sort
            [ "alunos/" ++ nome
            | nome <- arquivosAlunos
            , ".py" `isSufixoDe` nome
            ]

    mapM_ (\aluno -> corrigirTodosTestes aluno testes) alunos

isSufixoDe :: String -> String -> Bool
isSufixoDe sufixo texto =
    reverse sufixo == take (length sufixo) (reverse texto)
