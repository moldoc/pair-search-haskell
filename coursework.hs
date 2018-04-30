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
        
    fileloop files gap pairAmount Map.empty charList []
    finalResult <- takeMVar charList
    let result = prettify finalResult
    writeFile "output.txt" (unlines result) 
    
    
-- Returns a list of the files
findFiles :: [String] -> [String]
findFiles contents
    | length contents > 3 = drop 2 contents
    | otherwise = (last contents):[]
    
-- Prettifies the output for the file
prettify [] = []
prettify pairList =
    prettifiedPair:prettify (drop 1 pairList)
    where pair = pairList !! 0
          prettifiedPair= ((show (fst (fst pair))) ++ " " ++ (show (snd (fst pair))) ++ " " ++ (show (snd pair)))


fileloop files gap pairAmount chars charList maps =
    if length files > 0
        then do inFile <- readFile (files !! 0)
                forkIO $ do
                    let charMap = returnCharPairs inFile gap
                    fileloop (tail files) gap pairAmount charMap charList (charMap:maps)
                    exitSuccess
        else forkIO $ do
                let finalMap = mergeMaps maps
                putMVar charList (take pairAmount (sortMap finalMap))
                exitSuccess
                

-- Merge the maps in a list
mergeMaps [x] = x
mergeMaps mapList =
    mergeMaps ((Map.unionWith (+) (mapList !! 0) (mapList !! 1)):(drop 2 mapList))
    
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

                    
-- Find character pairs of a line with the given gap
findPair :: String -> Int -> [((Char,Char),Int)]
findPair line gap
    | length line > 1 = (getPair line gap) ++ findPair (tail line) gap
    | otherwise = []


getPair :: String -> Int -> [((Char,Char),Int)]
getPair line gap
    | gap > 0 && gap < length line = (((head line), line !! gap),1):getPair line (gap-1)
    | gap > 0 && gap >= length line = (((head line), line !! ((length line)-1)),1):getPair line ((length line)-2)
    | otherwise = []

    
-- Add together values on the same keys
-- Call this to get the eventual number of occurrences for each character pair
addValuesTogether :: (Num v, Ord c1, Ord c2) => [((c1,c2),v)] -> Map.Map (c1,c2) v
--(Num a, Num k, Ord k) => [(k,a)] -> Map.Map k a
addValuesTogether charList = Map.fromListWith (+) charList

sortMap charMap =
    sortBy (\((k1, k2),v1) ((k3, k4), v2) -> v2 `compare` v1) $ Map.toList charMap
