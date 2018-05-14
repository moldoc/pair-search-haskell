-- Note: my laptop's computing power isn't very good, so I tested this only
-- with input files each containing <=500 lines of text. Therefore I cannot
-- be certain how this program performs on really big files.

import Data.List
import qualified Data.Map as Map
import System.IO
import Control.Concurrent
import System.Exit

-- Main function that should be called to execute the program.
main = do
    iFile <- readFile "input.txt"
    let contents = lines iFile
    let gap = read (contents !! 0) :: Int
    let pairAmount = read (contents !! 1) :: Int
    let files = findFiles contents

    charList <- newEmptyMVar
    totalLines <- newEmptyMVar
        
    fileloop files 0 gap pairAmount charList []
    finalResult <- takeMVar charList
    let result = prettify (fst finalResult) (snd finalResult)
    writeFile "output.txt" (unlines result) 
    
    
-- Returns a list of the files
-- The first two items in the input list are the gap and number of pairs, so
-- they are dropped out.
findFiles :: [String] -> [String]
findFiles contents
    | length contents > 3 = drop 2 contents
    | otherwise = (last contents):[]
    
-- Prettifies the output for the file to be of form
-- 'c1' 'c2' onLines totalLines
prettify :: (Show a, Show a1, Show a2, Show a3) => [((a1, a), a2)] -> a3 -> [String]
prettify [] x = []
prettify pairList totalLines =
    prettifiedPair:prettify (drop 1 pairList) totalLines
    where pair = pairList !! 0
          prettifiedPair = ((show . fst . fst $ pair) ++ " " ++ (show . snd . fst $ pair) ++ " " ++ (show . snd $ pair) ++ " " ++ (show totalLines))

-- Goes through all the files in the input file and starts a thread
-- for each of them. Parameters: a list with the name of the files,
-- total number of lines in all of the files (starts with 0 and updates on every iteration)
-- the gap constraint, the number of most common pairs wanted for the output,
-- the MVar "charList" for enabling the sending of the final result to the MVar created
-- in the main function, the list containing all the maps received from the files (starts empty)
fileloop
    :: [FilePath]
     -> Int
     -> Int
     -> Int
     -> MVar ([((Char, Char), Int)], Int)
     -> [Map.Map (Char, Char) Int]
     -> IO ThreadId

fileloop files lineAmount gap pairAmount charList maps =
    if length files > 0
        then do inFile <- readFile (files !! 0)
                forkIO $ do
                    let fileLines = length (lines inFile)
                    let charMap = returnCharPairs inFile gap
                    fileloop (tail files) (lineAmount + fileLines) gap pairAmount charList (charMap:maps)
                    exitSuccess
        else forkIO $ do
                let finalMap = mergeMaps maps
                putMVar charList ((take pairAmount $ sortMap finalMap), lineAmount)
                exitSuccess
                

-- Merge the maps in a list
mergeMaps :: (Num a, Ord k) => [Map.Map k a] -> Map.Map k a
mergeMaps [x] = x
mergeMaps mapList =
    mergeMaps ((Map.unionWith (+) (mapList !! 0) (mapList !! 1)):(drop 2 mapList))
    
-- Return the charactermap of a file
returnCharPairs :: String -> Int -> Map.Map (Char, Char) Int
returnCharPairs inFile gap =
    addValuesTogether charPairs
    where charPairs = concat $ goThroughLines contents gap
          contents = lines inFile
                  
-- Return the unique character pairs in a file
goThroughLines :: [String] -> Int -> [[((Char, Char), Int)]]
goThroughLines [] gap = []
goThroughLines contents gap =
    newChars:(goThroughLines (tail contents) gap)
    -- If you want to include spaces in the result, replace noSpace with
    -- contents !! 0.
    -- nub is used to make sure that all character pairs are only saved once
    -- per line, since the final value is the number of lines they appeared on
    -- and NOT the number of the appearances of the character pair in total. 
    where newChars = nub (findPair noSpace gap)
          noSpace = filter isSpace (contents !! 0)
          
-- Two functions for deleting the spaces on a line.
isSpace char = not (char `elem` [' '])

filterSpaces line = filter isSpace line
                    
-- Two functions for finding character pairs of a line with the given gap
findPair :: String -> Int -> [((Char,Char),Int)]
findPair line gap
    -- Send gap+1 to the function getPair instead of gap, as we want
    -- the maximum number of characters between c1 and c2 to be the value of
    -- gap (indexing starts at 0)
    | length line > 1 = (getPair line (gap+1)) ++ findPair (tail line) gap
    | otherwise = []

-- If the gap is bigger than the remaining length of the line from c1,
-- the highest position for c2 is the length of the line instead of the gap.
getPair :: String -> Int -> [((Char,Char),Int)]
getPair line gap
    | gap > 0 && gap < length line = (((head line), line !! gap),1):getPair line (gap-1)
    | gap > 0 && gap >= length line = (((head line), line !! ((length line)-1)),1):getPair line ((length line)-2)
    | otherwise = []

    
-- Add together values on the same keys
-- Call this to get the eventual number of occurrences for each character pair
addValuesTogether :: (Num v, Ord c1, Ord c2) => [((c1,c2),v)] -> Map.Map (c1,c2) v
addValuesTogether charList = Map.fromListWith (+) charList

-- Convert character pair-value map to list sorted from biggest value to smallest
sortMap :: Ord a => Map.Map (t, t1) a -> [((t, t1), a)]
sortMap charMap =
    sortBy (\((k1, k2),v1) ((k3, k4), v2) -> compare v2 v1) $ Map.toList charMap
