module KWIC (generateKWIC) where

import Data.Char (toLower)
import Data.List (sort)

-- Monad to wrap values
newtype Identity a = Identity { runIdentity :: a }

-- Functor instance for Identity
instance Functor Identity where
    fmap f (Identity x) = Identity (f x)

-- Applicative instance for Identity
instance Applicative Identity where
    pure = Identity
    (Identity f) <*> (Identity x) = Identity (f x)

-- Monad instance for Identity
instance Monad Identity where
    (Identity x) >>= f = f x

-- Stop words list
stopWords :: [String]
stopWords = ["a", "o", "as", "os", "um", "uma", "é", "de", "em", "que", "para", "com", "the", "is", "of", "The"]

-- Everything into a single uniform monadic operation
generateKWIC :: [String] -> [(String, String)]
generateKWIC titles = concatMap processTitle titles
    where
        -- Process each title using the Identity monad
        processTitle :: String -> [(String, String)]
        processTitle title = 
            let titleWords = words title
            in runIdentity (filterStopWords titleWords >>= sortWords >>= generateShifts titleWords)

        -- Filter stop words from both ends of the title
        filterStopWords :: [String] -> Identity [String]
        filterStopWords wordsList = return (removeStopWordsFromEnds (filter (\word -> not (map toLower word `elem` stopWords)) wordsList))

        -- Remove stop words from both ends of the list
        removeStopWordsFromEnds :: [String] -> [String]
        removeStopWordsFromEnds = reverse . dropWhile (`elem` stopWords) . reverse . dropWhile (`elem` stopWords)

        -- Sort the filtered words
        sortWords :: [String] -> Identity [String]
        sortWords wordsList = return (sort wordsList)

        -- Generate circular shifts for each word in the filtered and sorted list
        generateShifts :: [String] -> [String] -> Identity [(String, String)]
        generateShifts title wordsList = 
            return (map (\kw -> (kw, circularShift title kw)) wordsList)

        -- Circular shift operation
        circularShift :: [String] -> String -> String
        circularShift title keyword = unwords (take len (dropWhile (/= keyword) title) ++ takeWhile (/= keyword) title)
            where len = length title
