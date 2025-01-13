module Main (main) where


import KWIC (generateCircularShifts, sortShifts, formatOutput, readFileContent, TheOne(..), unwrap)

import System.Environment (getArgs)
import Safe (headMay)

main :: IO ()
main = do
    -- Obtém os argumentos da linha de comando
    args <- getArgs

    -- Verifica se o nome do arquivo foi fornecido
    case headMay args of
        Nothing -> putStrLn "Por favor, forneça o caminho para o arquivo como argumento."
        Just filePath -> do
            -- Lê o conteúdo do arquivo
            content <- readFileContent filePath
            let linesContent = lines (unwrap content)  -- Convert string into list of lines
            -- Gera as rotações circulares (agora passando as linhas)
            let circularShifts = generateCircularShifts linesContent
            -- Ordena as rotações
            let sortedShifts = sortShifts circularShifts
            -- Formata a saída e imprime
            putStrLn (formatOutput sortedShifts)
