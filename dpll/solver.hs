import System.IO
import Data.Graph
import Data.Tree
import System.Environment
import Data.Time
import Data.List
import Data.Maybe

solve :: [[Int]] -> [(Int,[[Int]],[Int])] -> Bool -> [Int]
solve [] state backtrack = (map first state)
solve formula state backtrack =
  if backtrack then
    let clause = (third (head state))
        remainderState = (tail state)
        vb = (resolve clause (map first remainderState))
    in
    if (vb /= 0) then
      solve (removeClauses formula vb) ((vb,formula,(tail clause)):remainderState) False
    else
      let bl' = conflictIndices (head formula) (map first remainderState)
      in resolveConflict bl' remainderState
  else
    let assignments = (map first state)
        v = (resolve (head formula) assignments)
    in
    if v /= 0 then
      solve (removeClauses formula v) ((v,formula,(tail (head formula))):state) False
    else
      let bl = conflictIndices (head formula) (map first state)
      in resolveConflict bl state

resolveConflict :: [Int] -> [(Int,[[Int]],[Int])] -> [Int]
resolveConflict indices [] = solve [] [] True
resolveConflict [] state = solve (second (last state)) [(0,[],(third (last state)))] True
resolveConflict (x:xs) state =
  let rst = solve (second (state !! x)) (drop x state) True
  in
  if (rst == []) then resolveConflict xs state
  else rst


removeClauses :: [[Int]] -> Int -> [[Int]]
removeClauses [] v = []
removeClauses (x:xs) v =
  if v `elem` x then
    removeClauses xs v
  else
    x:(removeClauses xs v)

conflictIndices :: [Int] -> [Int] -> [Int]
conflictIndices clause [] = []
conflictIndices [] assignments = []
conflictIndices (c:lause) assignments
  | isJust (elemIndex (-c) assignments) = (fromJust $ (elemIndex (-c) assignments)):(conflictIndices lause assignments)
  | otherwise = conflictIndices lause assignments

resolve :: [Int] -> [Int] -> Int
resolve [] partialAssign = 0
resolve (x:xs) partialAssign
  | (-x) `elem` partialAssign = (resolve xs partialAssign)
  | otherwise = x

first :: (Int,[[Int]],[Int]) -> Int
first (a,b,c) = a

second :: (Int,[[Int]],[Int]) -> [[Int]]
second (a,b,c) = b

third :: (Int,[[Int]],[Int]) -> [Int]
third (a,b,c) = c

-- return a list of characters where each character is a digit
getNumStr :: [Char] -> [Char]
getNumStr [] = []
getNumStr(x:xs)
  | (x == '\t' || x == ' ') = []
  | otherwise = x : (getNumStr xs)

-- return a string that can be parsed
parseNext :: [Char] -> [Char]
parseNext [] = []
parseNext(x:xs)
  | (x == '\t' || x == ' ') = x:xs
  | otherwise = parseNext xs

-- return a list of integers
makeIntList :: [Char] -> [Int]
makeIntList [] = []
makeIntList(x:xs)
  | (x == '\t' || x == ' ') = (makeIntList xs)
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
  | (x == '\t') || (x == ' ') = parseVars xs (c+1)
  | ((x /= '\t' || x /= '\t') && (c == 2)) = read (getNumStr (x:xs)) :: Int
  | otherwise = parseVars xs c

removeComments :: [[Char]] -> [[Char]]
removeComments [] = []
removeComments(x:xs)
  | (head x) /= 'c' = x:removeComments xs
  | otherwise = removeComments xs

showIntList :: [Int] -> String
showIntList [] = ""
showIntList (x:xs) = do
  show x ++ " " ++ showIntList xs

main = do
        file <- getArgs
        start <- getCurrentTime
        if (length (file) == 1)
          then do
            contents <- readFile (head file)
            let newContents = (removeComments (lines contents))
                vars = (parseVars (head newContents) 0)
                parsed = (parseContent (tail newContents))
                solution = solve parsed [] False
            if solution == [] then
              putStrLn $ id "s UNSATISFIABLE"
            else
              mapM_ putStrLn ["s SATISFIABLE ","v " ++ showIntList solution]
          else
            print "Error : missing args DIMACS file"
        end <- getCurrentTime
        mapM_ putStrLn [("c Done with time " ++ (show (diffUTCTime end start)))]
