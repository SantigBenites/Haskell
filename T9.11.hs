import Data.Char
import System.IO 

-- le linha: imprime em Caps
main = do
    linha <- getLine
    putStrLn $ map toUpper linha
    main

-- Fazem o mesmo mas o getContents e recursivo por natureza
main' = do
    linha <- getContents
    putStrLn $ map toUpper linha

-- Fazem o mesmo mas o getContents e recursivo por natureza
main'' = do
    linha <- getContents
    putStrLn $ linhasCurtasApenas linha

linhasCurtasApenas :: String -> String
linhasCurtasApenas =
    unlines . filter (\linha -> length linha < 10) . lines

main''' = do
    handle <- openFile "t8-pre.hs" ReadMode -- Abrir Ficheiro
    conteudo <- hGetContents handle-- ler ficheiro
    putStrLn conteudo -- escrever conteudo
    hClose handle -- fechar ficheiro

main'''' = do
    conteudo <- readFile "t3.hs" -- le do ficheiro
    putStrLn conteudo -- escrever conteudo

main''''' = do
    conteudo <- readFile "t3.hs" -- le do ficheiro
    writeFile "t3-maisculas.hs" conteudo --escreve em ficheiro
