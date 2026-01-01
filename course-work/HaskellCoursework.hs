iLoveHaskell :: IO ()
iLoveHaskell = do
    putStrLn "Input integer number of times:"

    value <- getLine
    let number = read value :: Int

    if isvalid number
        then do
            sequence_ [putStrLn "I love Haskell!" | _ <- [1..number]]
        else do
            putStrLn "Invalid value inputted. Try Again."
            iLoveHaskell

    where
        isvalid :: Int -> Bool
        isvalid v
            | v >= 0    = True
            | otherwise = False


data STree = Leaf String | Node STree String STree deriving Show

tDepth :: STree -> Int
tDepth t = tDepth_ t 0

tDepth_ :: STree -> Int -> Int
tDepth_ (Leaf _) acc = acc
tDepth_ (Node left _ right) acc = max (tDepth_ left (succ acc)) (tDepth_ right (succ acc))


tMirror :: STree -> STree
tMirror (Leaf x) = Leaf x
tMirror (Node left y right) = Node (tMirror right) y (tMirror left)

prop_depth :: STree -> Bool
prop_depth t = (tDepth . tMirror) t == tDepth t

-- proof of prop_depth
-- 
-- pre-conditions:
-- tDepth is always >= 0
-- 
-- 
-- 
-- 
-- 
-- I.H.
-- (tDepth . tMirror) t == tDepth t
-- 
-- base case:
-- LHS:
-- (tDepth . tMirror) (Leaf x) 
-- = tDepth (tMirror (Leaf x))                                      by def .  
-- = tDepth (Leaf x)                                                by def tMirror
-- = 1                                                              by def tDepth
--                      
-- RHS:                     
-- tDepth (Leaf x)                      
-- = 1                                                              by def tDepth
-- = LHS                        
--                      
-- inductive case:                      
-- LHS:                     
-- (tDepth . tMirror) (Node x y z)                      
-- = tDepth (tMirror (Node x y z))                                  by def .
-- = tDepth (Node (tMirror z) y (tMirror x))                        by def tMirror
-- = max (tDepth_ left (succ acc)) (tDepth_ right (succ acc))       by 
-- 
-- 
