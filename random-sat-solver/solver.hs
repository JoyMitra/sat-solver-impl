import System.Random
import System.IO
import System.Environment
import System.Timeout
import Data.Time

getAssignment :: (Ord a, Num a) => [[a]] -> Int -> [a]
getAssignment [] n = []
getAssignment (x:xs) n =
  let pa = (getAssignment xs (n+100))
  in
    if ((length (filter (`elem` pa) x)) == 0) then
      let r = assignRandom (removeOpp pa x) (mkStdGen n)
      in
        if (r == 0) then [] else r:pa
    else
      pa

assignRandom :: (Ord a, Num a) => [a] -> StdGen -> a
assignRandom [] gen = 0
assignRandom x gen
  | (length x) == 1 = x !! 0
  | (length x) > 1 = x !! (fst (randomR (1,1989889) gen) `mod` (length x))

-- contradicts :: (Ord a, Num a) => a -> [a] -> Bool
-- contradicts n [] = False
-- contradicts n (x:xs) = if (n == x) then True else (contradicts n xs)

removeOpp :: (Ord a, Num a) => [a] -> [a] -> [a]
removeOpp [] cl = cl
removeOpp (x:xs) cl = removeOpp xs (filter (/= (-x)) cl)

verify :: (Ord a, Num a) => [[a]] -> [a] -> Bool
verify f [] = False
verify [] am = True
verify (f:fs) am
  | (length (filter (`elem` am) f)) == 0 = False
  | otherwise = verify fs am

solve :: (Ord a, Num a) => Int -> [[a]] -> [a]
solve n [] = []
solve 0 x = []
solve n x = let am = (getAssignment x n)
            in
              if (verify x am) then am else solve (n-1) x

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
        if (length (file) == 2)
          then do
            contents <- readFile (head file)
            let newContents = (removeComments (lines contents))
                vars = (parseVars (head newContents) 0)
                parsed = (parseContent (tail newContents))
                maxTries = read (head (tail file)) :: Int
                solution = solve maxTries parsed
            if solution == [] then
              putStrLn $ id "s UNSATISFIABLE"
            else
              mapM_ putStrLn ["s SATISFIABLE ","v " ++ showIntList (tail solution)]
          else
            print "Req args missing : DIMACS file, MaxTries"
        end <- getCurrentTime
        mapM_ putStrLn [("c Done with time " ++ (show (diffUTCTime end start)))]

main :: IO ()
main = do
res <- timeout 600000000 worker
case res of
  Nothing -> putStrLn "c solver terminated with no results after 10 minutes"
  Just () -> putStrLn "c solver ran to completion"
