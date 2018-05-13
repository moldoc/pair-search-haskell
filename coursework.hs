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
    totalLines <- newEmptyMVar
        
    fileloop files 0 gap pairAmount Map.empty charList []
    finalResult <- takeMVar charList
    let result = prettify (fst finalResult) (snd finalResult)
    writeFile "output.txt" (unlines result) 
    
    
-- Returns a list of the files
findFiles :: [String] -> [String]
findFiles contents
    | length contents > 3 = drop 2 contents
    | otherwise = (last contents):[]
    
-- Prettifies the output for the file
prettify :: (Show a, Show a1, Show a2, Show a3) => [((a1, a), a2)] -> a3 -> [String]
prettify [] x = []
prettify pairList totalLines =
    prettifiedPair:prettify (drop 1 pairList) totalLines
    where pair = pairList !! 0
          prettifiedPair= ((show . fst . fst $ pair) ++ " " ++ (show . snd . fst $ pair) ++ " " ++ (show . snd $ pair) ++ " " ++ (show totalLines))

fileloop
    :: [FilePath]
     -> Int
     -> Int
     -> Int
     -> Map.Map (Char, Char) Int
     -> MVar ([((Char, Char), Int)], Int)
     -> [Map.Map (Char, Char) Int]
     -> IO ThreadId

fileloop files lineAmount gap pairAmount chars charList maps =
    if length files > 0
        then do inFile <- readFile (files !! 0)
                forkIO $ do
                    let fileLines = length (lines inFile)
                    let charMap = returnCharPairs inFile gap
                    fileloop (tail files) (lineAmount + fileLines) gap pairAmount charMap charList (charMap:maps)
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
    where newChars = nub (findPair noSpace gap)
          noSpace = filter isSpace (contents !! 0)
	      
-- Two functions for deleting the spaces on a line
isSpace char = not (char `elem` [' '])

filterSpaces line = filter isSpace line
                    
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
addValuesTogether charList = Map.fromListWith (+) charList

sortMap :: Ord a => Map.Map (t, t1) a -> [((t, t1), a)]
sortMap charMap =
    sortBy (\((k1, k2),v1) ((k3, k4), v2) -> v2 `compare` v1) $ Map.toList charMap
