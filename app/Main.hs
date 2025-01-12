module Main (main) where

import KWIC (generateKWIC)

main :: IO ()
main = do
    -- Read input from input.text
    content <- readFile "input.txt"
    let titles = lines content
    let kwic = generateKWIC titles

    -- Format KWIC output as strings
    let formattedOutput = unlines $ map (\(kw, ctx) -> kw ++ " -> " ++ ctx) kwic

    -- Write output to output.txt
    writeFile "output.txt" formattedOutput

    putStrLn "KWIC has been written into output.txt"
