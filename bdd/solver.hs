import System.IO
import System.Environment
import System.Timeout
import Data.Time

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

restrict :: (Ord a, Num a) => a -> Tree a -> Tree a
restrict a (Node root left right)
  | a > 0 = right
  | a < 0 = left

varTree :: (Ord a, Num a) => a -> Tree a
varTree a
  | a == 0 = EmptyTree
  | a > 0 = Node a (Node 0 EmptyTree EmptyTree) (Node 1 EmptyTree EmptyTree)
  | a < 0 = Node (-a) (Node 1 EmptyTree EmptyTree) (Node 0 EmptyTree EmptyTree)

applyOR :: (Ord a, Num a) => Tree a -> Tree a -> Tree a
applyOR EmptyTree t2 = t2
applyOR t1 EmptyTree = t1
applyOR (Node a EmptyTree EmptyTree) t2
  | a == 0 = t2
  | a == 1 = (Node a EmptyTree EmptyTree)
applyOR t1 (Node a EmptyTree EmptyTree)
  | a == 0 = t1
  | a == 1 = (Node a EmptyTree EmptyTree)
applyOR (Node a left right) (Node a' left' right')
  | a == a' = let t1 = (Node a left right)
                  t2 = (Node a' left' right')
              in (Node a (applyOR (restrict (-a) t1) (restrict (-a) t2))
                                (applyOR (restrict a t1) (restrict a t2)))
  | a < a' = let t1 = (Node a left right)
                 t2 = (Node a' left' right')
              in (Node a (applyOR (restrict (-a) t1) t2)
                            (applyOR (restrict a t1) t2))
  | a > a' = let t1 = (Node a left right)
                 t2 = (Node a' left' right')
              in (Node a' (applyOR t1 (restrict (-a') t2))
                            (applyOR t1 (restrict a' t2)))

applyAND :: (Ord a, Num a) => Tree a -> Tree a -> Tree a
applyAND EmptyTree t2 = EmptyTree
applyAND t1 EmptyTree = EmptyTree
applyAND (Node a EmptyTree EmptyTree) t2
  | a == 0 = (Node a EmptyTree EmptyTree)
  | a == 1 = t2
applyAND t1 (Node a EmptyTree EmptyTree)
  | a == 0 = (Node a EmptyTree EmptyTree)
  | a == 1 = t1
applyAND (Node a left right) (Node a' left' right')
  | a == a' = let t1 = (Node a left right)
                  t2 = (Node a' left' right')
              in (Node a (applyAND (restrict (-a) t1) (restrict (-a) t2))
                                (applyAND (restrict a t1) (restrict a t2)))
  | a < a' = let t1 = (Node a left right)
                 t2 = (Node a' left' right')
              in (Node a (applyAND (restrict (-a) t1) t2)
                            (applyAND (restrict a t1) t2))
  | a > a' = let t1 = (Node a left right)
                 t2 = (Node a' left' right')
              in (Node a' (applyAND t1 (restrict (-a') t2))
                            (applyAND t1 (restrict a' t2)))

bddClause :: (Ord a,Num a) => [Tree a] -> Tree a
bddClause [] = EmptyTree
bddClause (x:[]) = x
bddClause (x:y:[]) = applyOR x y
bddClause (x:y:z) = applyOR (applyOR x y) (bddClause z)

bddFormula :: (Ord a,Num a) => [[a]] -> Tree a
bddFormula [] = (Node 0 EmptyTree EmptyTree)
bddFormula (x:[]) = bddClause (map varTree x)
bddFormula (x:y:[]) = applyAND (bddClause (map varTree x)) (bddClause (map varTree y))
bddFormula (x:y:z) = applyAND (applyAND (bddClause (map varTree x)) (bddClause (map varTree y))) (bddFormula z)

isSat :: (Ord a,Num a) => Tree a -> Bool
isSat EmptyTree = False
isSat (Node 1 EmptyTree EmptyTree) = True
isSat (Node 0 EmptyTree EmptyTree) = False
isSat (Node a left right) =
  if ((isSat left) || (isSat right)) then True else False

model :: (Ord a,Num a) => Tree a -> [a]
model EmptyTree = []
model (Node 1 EmptyTree EmptyTree) = [1]
model (Node 0 EmptyTree EmptyTree) = []
model (Node a left right) =
  if ((model left) /= []) then
    ((model left) ++ [-a])
  else
    if ((model right) /= []) then
      ((model right) ++ [a])
    else
      []

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
        if (length (file) == 1)
          then do
            contents <- readFile (head file)
            let newContents = (removeComments (lines contents))
                vars = (parseVars (head newContents) 0)
                parsed = (parseContent (tail newContents))
                solution = model (bddFormula parsed)
            if solution == [] then
              putStrLn $ id "s UNSATISFIABLE"
            else
              mapM_ putStrLn ["s SATISFIABLE ","v " ++ showIntList (tail solution)]
          else
            print "Error : missing args DIMACS file"
        end <- getCurrentTime
        mapM_ putStrLn [("c Done with time " ++ (show (diffUTCTime end start)))]

main :: IO ()
main = do
res <- timeout 600000000 worker
case res of
  Nothing -> putStrLn "c solver terminated with no results after 10 minutes"
  Just () -> putStrLn "c solver ran to completion"
