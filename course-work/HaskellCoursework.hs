iLoveHaskell :: IO ()
iLoveHaskell = do
    putStrLn "Input number of times:"

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
tDepth (Leaf _) = 0
tDepth (Node left _ right) = 1 + max (tDepth left) (tDepth right)

tMirror :: STree -> STree
tMirror (Leaf x) = Leaf x
tMirror (Node left y right) = Node (tMirror right) y (tMirror left)
 
-- proof of prop_depth:
-- 
--     where:
--         prop_depth :: STree -> Bool
--         prop_depth t = (tDepth . tMirror) t == tDepth t
--     
--     I.H.
--         (tDepth . tMirror) t == tDepth t
--     
--     base case:
--         LHS:
--             (tDepth . tMirror) (Leaf x) 
--             = tDepth (tMirror (Leaf x))                                  by def .  
--             = tDepth (Leaf x)                                            by def tMirror
--             = 0                                                          by def tDepth
--       
--         RHS:  
--             tDepth (Leaf x)   
--             = 0                                                          by def tDepth
--             = LHS     
--           
--     
--     inductive case:    
--         LHS:  
--             (tDepth . tMirror) (Node left x right)   
--             = tDepth (tMirror (Node left x right))                       by def .
--             = tDepth (Node (tMirror right) x (tMirror left))             by def tMirror
--             = 1 + max (tDepth (tMirror right)) (tDepth (tMirror left))   by def tDepth
--             = 1 + max (tDepth right) (tDepth left)                       by I.H.
--         RHS:
--             tDepth (Node left x right)
--             = 1 + max (tDepth left) (tDepth right)                       by def tDepth
--             = LHS                                                        by commutativity
 



-- pre-condition  : 
--     n >= 0
-- post-condition : 
--     trib n = trib (n - 1) + trib (n - 2) + trib (n - 3)
--     0 <= trib (n - 3) < trib (n - 2) < trib (n - 1) < trib n

trib :: Integral a => a -> a
trib 0 = 0
trib 1 = 0
trib 2 = 1
trib n = trib (n - 1) + trib (n - 2) + trib (n - 3)


trib2 :: Integral a => a -> a
trib2 n = trib2_ n 0 0 1

trib2_ :: Integral a => a -> a -> a -> a -> a
trib2_ n a b c = if n > 0
                    then
                        trib2_ (n - 1) b c (a + b + c)
                    else
                        a

-- proof of prop_tribonacci:
-- 
--     where:
--         prop_tribonacci :: Integral a => a -> Bool
--         prop_tribonacci n = (trib n) == (trib2 n)
--     
--     I.H.
--         (trib n) == (trib2 n)
-- 
--     base case:
--         LHS:
--             trib 0
--             = 0                                                          by def trib
--         RHS:
--             trib2 0
--             = trib2_ 0 0 0 1                                             by def trib2
--             = 0                                                          by def trib2_
--             = LHS
-- 
--     inductive case:
--         LHS:
-- 

-- int trib2_while(int n) {
-- 
--   int a, b, c, temp
-- 
--   a = 0
--   b = 0
--   c = 1
--   
--   while (n > 0) {
--       temp = c
--       c = a + b + c
--       a = b
--       b = c
--   
--       n -= 1
--   }
-- 
--   return a
-- }
