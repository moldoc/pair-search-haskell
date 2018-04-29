import Data.List
import qualified Data.Map as Map
import System.IO
import Control.Concurrent
import System.Exit

main = do
    iFile <- readFile "input.txt"
    let contents = lines iFile
    let gap = read (contents !! 0) :: Int
    let pairAmount = read (contents !! 1) :: Int
    let files = findFiles contents

    charList <- newEmptyMVar
        
    fileloop files gap pairAmount Map.empty charList
    finalResult <- takeMVar charList
    putStrLn (show finalResult)
    

-- Returns a list of the files
findFiles :: [String] -> [String]
findFiles contents
    | length contents > 3 = drop ((length contents)-2) contents
    | otherwise = (last contents):[]


fileloop files gap pairAmount chars charList =
    if length files > 0
        then do inFile <- readFile (files !! 0)
                forkIO $ do
                    let charMap = returnCharPairs inFile gap
                    fileloop (tail files) gap pairAmount charMap charList
                    exitSuccess
                
        -- output (getHighestPairs charMap pairNumber)
        else forkIO $ do
                putMVar charList (getHighestPairs chars pairAmount)
                exitSuccess
                
            
-- Return the charactermap of a file
returnCharPairs inFile gap =
    addValuesTogether charPairs
    where charPairs = concat (goThroughLines contents gap)
          contents = lines inFile
                  
-- Return the unique character pairs in a file
goThroughLines [] gap = []
goThroughLines contents gap =
    newChars:(goThroughLines (tail contents) gap)
    where newChars = nub (findPair (contents !! 0) gap)

                    
-- function for finding character pairs of a line with the given gap
findPair :: String -> Int -> [((Char,Char),Int)]
findPair line gap
    | length line > 1 = (getPair line gap) ++ findPair (tail line) gap
    | otherwise = []


getPair :: String -> Int -> [((Char,Char),Int)]
getPair line gap
    | gap > 0 && gap < length line = (((head line), line !! gap),1):getPair line (gap-1)
    | gap > 0 && gap >= length line = (((head line), line !! ((length line)-1)),1):getPair line ((length line)-2)
    | otherwise = []
    
    
getUniques :: [(Char,Char)] -> [(Char,Char)]
getUniques charPairs = nub charPairs

    
-- Add together values on the same keys
-- Call this to get the eventual number of occurrences for each character pair
addValuesTogether :: (Num v, Ord c1, Ord c2) => [((c1,c2),v)] -> Map.Map (c1,c2) v
--(Num a, Num k, Ord k) => [(k,a)] -> Map.Map k a
addValuesTogether charList = Map.fromListWith (+) charList


-- Get the highest value(s)
-- Call this after addValuesTogether
getHighestValue charMap = filter is_biggest sorted
    where sorted = sortBy (\((k1, k2),v1) ((k3, k4), v2) -> v2 `compare` v1) $ Map.toList charMap
          max_v = snd $ head sorted
          is_biggest ((key1, key2),value) = value == max_v

-- Get the highest values
-- 1. Take the map of character pairs
-- 2. Call getHighestValue
-- 3. If getHighestValue returns a map whose length >= 5, take 5 first pairs and return
-- 4. If the map's length < 5, take the item's it has, and call getHighestValue with a map
--    which no longer has the character pairs the first call returned
getHighestPairs charMap pairNumber
    | listLength >=pairNumber = take pairNumber highestList
    | otherwise = take listLength highestList
    where highestList = getHighestValue charMap
          listLength = length highestList
