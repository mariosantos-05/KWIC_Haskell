module KWIC (
    generateCircularShifts,
    sortShifts,
    removeStopWords,
    splitWords,
    formatOutput,
    readFileContent,
    TheOne(..)
) where

import Data.List (sort)
import Data.Char (toLower, isAlpha)
import qualified Data.Set as Set

-- Definição da abstração TheOne
newtype TheOne a = TheOne { unwrap :: a }

instance Functor TheOne where
    fmap f (TheOne x) = TheOne (f x)

instance Applicative TheOne where
    pure = TheOne
    (TheOne f) <*> (TheOne x) = TheOne (f x)

instance Monad TheOne where
    return = pure
    (TheOne x) >>= f = f x

-- Funções auxiliares
readFileContent :: FilePath -> IO (TheOne String)
readFileContent path = fmap TheOne (readFile path)

splitWords :: String -> [String]
splitWords = words . map (\c -> if isAlpha c || c == ' ' then c else ' ')

removeStopWords :: [String] -> [String]
removeStopWords wordsList =
    let stopWords = Set.fromList ["a", "o", "as", "os", "um", "uma", "é", "de", "e", "da", "do", "The", "and", "an", "or", "is", "but", "so", "yet", "to", "nor", "on", "the"]
    in filter (\word -> not (map toLower word `Set.member` stopWords)) wordsList

generateCircularShifts :: [String] -> [(String, String)]
generateCircularShifts titles = 
    [ (unwords shifted, title) 
    | title <- titles
    , let wordsInTitle = splitWords title
    , let validWords = removeStopWords wordsInTitle
    , word <- validWords
    , let (before, after) = break (== word) wordsInTitle
    , let shifted = after ++ before
    ]

sortShifts :: [(String, String)] -> [(String, String)]
sortShifts = sort

formatOutput :: [(String, String)] -> String
formatOutput = unlines . map (\(shifted, original) -> shifted ++ " (from \"" ++ original ++ "\")")
