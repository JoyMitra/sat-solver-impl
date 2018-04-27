import System.Random
import Data.List
import System.IO
import System.Environment
import System.Timeout
import Data.Time

sanitize x y = if (x == -(y)) || (x == y) then True else False
min' x y = if ((snd x) < (snd y)) then x else y

randomAssignment :: Int -> Int -> [Int]
randomAssignment 0 try = []
randomAssignment nvars 0 = []
randomAssignment nvars try =
  delete 0 (nubBy sanitize (take nvars (randomRs (-(nvars),nvars) (mkStdGen try))))

getSatClauses :: [Int] -> [[Int]] -> [[Int]]
getSatClauses [] formula = []
getSatClauses model [] = []
getSatClauses model (f:fs) =
  if ((intersect model f) /= [])
    then (f:(getSatClauses model fs))
    else (getSatClauses model fs)

verify :: [Int] -> [[Int]] -> Bool
verify [] formula = False
verify model [] = False
verify model formula =
  if (length (getSatClauses model formula) == (length formula))
    then True
    else False

getUnsatClauses :: [Int] -> [[Int]] -> [[Int]]
getUnsatClauses [] formula = formula
getUnsatClauses model [] = []
getUnsatClauses model (f:fs) =
  if ((intersect model f) == [])
    then f:(getUnsatClauses model fs)
    else (getUnsatClauses model fs)

getNewModel :: [Int] -> Int -> [Int]
getNewModel [] v = [v]
getNewModel model v = v:(delete (-(v)) model)

chooseModelWithZeroBCVar :: [[Int]] -> [Int] -> [Int] -> [Int]
chooseModelWithZeroBCVar [] model randomUnsatClause = []
chooseModelWithZeroBCVar satClauses [] randomUnsatClause = []
chooseModelWithZeroBCVar satClauses model [] = []
chooseModelWithZeroBCVar satClauses model randomUnsatClause =
  let newModel = (getNewModel model (head randomUnsatClause))
  in if ((getUnsatClauses newModel satClauses) == [])
    then newModel
    else chooseModelWithZeroBCVar satClauses model (tail randomUnsatClause)

getVarsWithBC :: [[Int]] -> [Int] -> [Int] -> [(Int,Int)]
getVarsWithBC [] model randomUnsatClause = []
getVarsWithBC satClauses [] randomUnsatClause = []
getVarsWithBC satClauses model [] = []
getVarsWithBC satClauses model randomUnsatClause =
  let newModel = (getNewModel model (head randomUnsatClause))
      unsatClauses = (getUnsatClauses newModel satClauses)
  in ((head randomUnsatClause),(length unsatClauses)):(getVarsWithBC satClauses model (tail randomUnsatClause))


sls :: Double -> Int -> [Int] -> [[Int]] -> [Int]
sls p 0 model formula = if (verify model formula) then model else []
sls p nflip [] formula = []
sls p nfip model [] = []
sls p nflip model formula =
  if (verify model formula)
    then model
    else
      let unsatClauses = (getUnsatClauses model formula)
          randomUnsatClause = unsatClauses !! (fst (randomR (0,((length unsatClauses)-1)) (mkStdGen nflip)))
          satClauses = (getSatClauses model formula)
          satClausesCount = length satClauses
          modelWithZeroBCVar = chooseModelWithZeroBCVar satClauses model randomUnsatClause
      in if ((length modelWithZeroBCVar) /= 0)
        then sls p (nflip-1) modelWithZeroBCVar formula
        else let rp = (fst (randomR (0,1) (mkStdGen nflip))) :: Double
        in if (rp <= p)
          then getNewModel model (randomUnsatClause !! (fst (randomR (0,((length randomUnsatClause)-1)) (mkStdGen nflip))))
          else let v = (fst (foldl min' (0,(length formula)) (getVarsWithBC satClauses model randomUnsatClause)))
          in sls p (nflip-1) (getNewModel model v) formula

solve :: [[Int]] -> Int -> Int -> Int -> Double -> [Int]
solve formula nvars maxFlips 0 p = sls p maxFlips (randomAssignment nvars 0) formula
solve formula nvars maxFlips maxTries p =
  let result = (sls p maxFlips (randomAssignment nvars maxTries) formula)
  in if (result /= [])
    then result
    else solve formula nvars maxFlips (maxTries-1) p

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

worker :: IO ()
worker = do
        file <- getArgs
        start <- getCurrentTime
        if (length (file) == 4)
          then do
            contents <- readFile (head file)
            let newContents = (removeComments (lines contents))
                vars = (parseVars (head newContents) 0)
                parsed = (parseContent (tail newContents))
                maxFlips = read (file !! 1) :: Int
                maxTries = read (file !! 2) :: Int
                noise = read (file !! 3) :: Double
                solution = solve parsed vars maxFlips maxTries noise
            if (verify solution parsed) then
              mapM_ putStrLn ["s SATISFIABLE ","v " ++ showIntList (tail solution)]
            else
              putStrLn $ id "s UNSATISFIABLE"
          else
            print "Req args missing : <cnf file> <max flips> <max tries> <noise>"
        end <- getCurrentTime
        mapM_ putStrLn [("c Done with time " ++ (show (diffUTCTime end start)))]

main :: IO ()
main = do
res <- timeout 600000000 worker
case res of
  Nothing -> putStrLn "c solver terminated with no results after 10 minutes"
  Just () -> putStrLn "c solver ran to completion"
