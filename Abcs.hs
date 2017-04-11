module Abcs where

-- | Find all non-contiguous abc indexes
--
-- >>> abcs "acbaababba"
-- []
--
-- >>> abcs "abc"
-- [(0,1,2)]
--
-- >>> abcs "abbc"
-- [(0,2,3),(0,1,3)]
--
-- >>> unabcs "abbcc" [(0,1,3),(0,2,4)]
-- ["abc","abc"]
--
-- >>> all (=="abc") (unabcs "abcacbc" (abcs "abcacbc"))
-- True

abcs :: [Char] -> [(Int,Int,Int)]
abcs =
  flip abcs' 0

abcs' :: [Char] -> Int -> [(Int,Int,Int)]
abcs' [] _ =
  []
abcs' ('a':s) i =
  abcs' s (i+1) ++ bcs s i (i+1)
abcs' (_:s) i =
  abcs' s (i+1)

bcs :: [Char] -> Int -> Int -> [(Int,Int,Int)]
bcs [] _ _ =
  []
bcs ('b':s) ai bi =
  bcs s ai (bi+1) ++ cs s ai bi (bi+1)
bcs (_:s) ai bi =
  bcs s ai (bi+1)

cs :: [Char] -> Int -> Int -> Int -> [(Int,Int,Int)]
cs [] _ _ _ =
  []
cs ('c':s) ai bi ci =
  cs s ai bi (ci+1) ++ [(ai,bi,ci)]
cs (_:s) ai bi ci =
  cs s ai bi (ci+1)

unabcs :: [Char] -> [(Int,Int,Int)] -> [[Char]]
unabcs _ [] =
  []
unabcs s ((ai,bi,ci):is) =
  [(s !! ai),(s !! bi),(s !! ci)]:(unabcs s is)
