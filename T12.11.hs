import System.IO
import Data.List

main' = do
    --(commando:parametros) <- getArgs
    --dispatch commando parametros

dispatch :: String -> [String] -> IO()
dispatch "adiconar" [file ,tarefa] = appendFile file (tarefa ++ "\n")
dispatch "apagar" [] = return()
dispatch "listar" [] = return()


main = do
    conteudo <- readFile "tarefas.txt"
    let linhas = lines conteudo
        tarefas = zipWith (\n l -> show n ++ ": " ++ l) [0 ..] linhas
    mapM putStrLn tarefas
    putStrLn "qual a tarefa a apagar"
    numeroString <- getLine -- readLn
    let numero = read numeroString :: Int
        novasTarefas = unlines $ delete (linhas !! numero) linhas
    (tempNome , tempHandle) <- openTempFile "." "temp"
    hPutStrLn tempHandle novasTarefas
    hClose tempHandle
    --removeFile "tarefas.txt"
    --removeFile tempNome "tarefas.txt"

randomInteiro :: Int
randomInteiro = 4
