import Data.List (sort, tails, isPrefixOf)

rotations :: String -> [String]
rotations line = 
    let wordsList = words line
    in [unwords (drop i wordsList ++ take i wordsList) | i <- [0..length wordsList - 1]]


kwic :: [String] -> [String]
kwic lines = 
    let allRotations = concatMap rotations lines
        stopWords = ["a", "the", "is", "of", "A", "The", "Is", "Of"]
        filteredRotations = filter (\line -> not $ any (`isPrefixOf` line) stopWords) allRotations
    in sort filteredRotations


main :: IO ()
main = do
    let inputLines = ["The quick brown fox", "A brown cat sat","The cat is brown"]
    let kwicIndex = kwic inputLines
    mapM_ putStrLn kwicIndex
