import System.Random
import System.Environment
import System.IO
import Data.List

getClause :: [Int] -> [Int] -> [Int]
getClause [] x = [0]
getClause (x:xs) y = (y !! x) : (getClause xs y)

vars :: Int -> [Int]
vars 0 = []
vars n = n:(-n):(vars (n-1))

lrandoms r l = take l $ ((filter (>0)) (randoms (mkStdGen r)))

lrandomIndex :: Int -> [Int] -> [Int]
lrandomIndex 0 x = []
lrandomIndex nvars [] = []
lrandomIndex nvars (x:xs) = (x `mod` nvars) : (lrandomIndex nvars xs)

rmOpp :: [Int] -> [Int] -> [Int]
rmOpp [] [] = []
rmOpp x [] = []
rmOpp [] x = []
rmOpp x (y:ys) =
  if ((y `elem` x) && ((-y) `elem` x))
    then (if (y > 0) then (y:(rmOpp x ys)) else ((-y):(rmOpp x ys)))
    else (y:(rmOpp x ys))

stRandomCNF :: Int -> Int -> Int -> [[Int]]
stRandomCNF 0 k l = []
stRandomCNF n k l =
  (rmOpp (getClause (lrandomIndex (length (vars k)) (lrandoms n l)) (vars k))
  (getClause (lrandomIndex (length (vars k)) (lrandoms n l)) (vars k)))
  : (stRandomCNF (n-1) k l)

randomCNF :: Int -> Int -> Int -> [[Int]]
randomCNF 0 k l = []
randomCNF n k l =
  let (rnum, gen) = random (mkStdGen n)
      randl =
        if ((rnum `mod` l) < 0)
          then (-(rnum `mod` l))
          else ((rnum `mod` l)+1)
  in (rmOpp (getClause (lrandomIndex (length (vars k)) (lrandoms n randl)) (vars k))
     (getClause (lrandomIndex (length (vars k)) (lrandoms n randl)) (vars k)))
     : (randomCNF (n-1) k l)

intercalate' :: [Int] -> String
intercalate' [] = ""
intercalate' (x:xs) = (show x) ++ "\t" ++ (intercalate' xs)

collapse :: [[Int]] -> String
collapse [] = ""
collapse (x:xs) = (intercalate' x) ++ "\n" ++ (collapse xs)

main = do (f:x:y:z) <- getArgs
          writeFile f ("p" ++ "\t" ++ "cnf" ++ "\t" ++ y ++ "\t" ++ x ++ "\n")
          let n = read x :: Int
          let k = read y :: Int
          let l = read (head z) :: Int
          if (((length z) == 2) && ("strict" == (last z)))
            then appendFile f (collapse (stRandomCNF n k l))
            else if ((length z) == 1)
                    then appendFile f (collapse (randomCNF n k l))
                    else print "wrong usage"
