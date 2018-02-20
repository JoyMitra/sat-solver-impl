import System.IO
import Data.Graph
import Data.Tree
import System.Environment

vertexList :: Int -> [(Int,Int)]
vertexList 0 = []
vertexList n
  | n < 0 = []
  | otherwise = (n,-n):(vertexList (n-1))

edgeList :: [[Int]] -> [(Int,Int)]
edgeList [] = []
edgeList (x:xs) =
  let v1 = -(head x)
      v2 = (last x)
      v3 = (head x)
      v4 = -(last x)
  in (v1,v2):(v4,v3):(edgeList xs)

isContradiction :: Tree Vertex -> [(Int,Int)] -> Bool
isContradiction t [] = False
isContradiction t (p:ps)
  | (fst p) `elem` t && (snd p) `elem` t = True
  | otherwise = isContradiction t ps

isUnsat :: Forest Vertex -> [(Int,Int)] -> Bool
isUnsat [] [] = False
isUnsat [] p = False
isUnsat f [] = False
isUnsat (x:xs) p
  | (isContradiction x p) = True
  | otherwise = isUnsat xs p

-- return a list of characters where each character is a digit
getNumStr :: [Char] -> [Char]
getNumStr [] = []
getNumStr(x:xs)
  | x == '\t' = []
  | otherwise = x : (getNumStr xs)

-- return a string that can be parsed
parseNext :: [Char] -> [Char]
parseNext [] = []
parseNext(x:xs)
  | x == '\t' = x:xs
  | otherwise = parseNext xs

-- return a list of integers
makeIntList :: [Char] -> [Int]
makeIntList [] = []
makeIntList(x:xs)
  | x == '\t' = (makeIntList xs)
  | x == '0' = (makeIntList xs)
  | x == '-' = (-(read (getNumStr xs) :: Int)) : (makeIntList (parseNext xs))
  | otherwise = (read (getNumStr (x:xs)) :: Int) : (makeIntList (parseNext xs))

-- transforms a list of strings to a list of integer lists
parseContent :: [[Char]] -> [[Int]]
parseContent [] = []
parseContent(x:xs) = makeIntList x : parseContent xs

-- returns the no. of variables in a DIMACS file
parseVars :: [Char] -> Int -> Int
parseVars [] c = 0
parseVars(x:xs) c
  | x == '\t' = parseVars xs (c+1)
  | ((x /= '\t') && (c == 2)) = read (getNumStr (x:xs)) :: Int
  | otherwise = parseVars xs c


main = do
        (opt:file) <- getArgs
        if (length (opt:file)) == 2 && opt == "-f"
          then do
            contents <- readFile (head file)
            let vars = (parseVars (head (lines contents)) 0)
                graph = (buildG (-vars,vars) (edgeList (parseContent (tail (lines contents)))))
            if (isUnsat (scc graph) (vertexList vars))
              then print "The formula is unsat"
              else print "The formula is sat"
          else
            print "-f <file path> is mandatory"
