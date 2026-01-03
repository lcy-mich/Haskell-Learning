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
tDepth (Leaf _) = 0
tDepth (Node left _ right) = 1 + max (tDepth left) (tDepth right)

tMirror :: STree -> STree
tMirror (Leaf x) = Leaf x
tMirror (Node left x right) = Node (tMirror right) x (tMirror left)
 
-- proof of (tDepth . tMirror) t == tDepth t:
--
--     base case, t = (Leaf x):
--         LHS:
--             (tDepth . tMirror) (Leaf x) 
--             = tDepth (tMirror (Leaf x))                                     by def .  
--             = tDepth (Leaf x)                                               by def tMirror
--             = 0                                                             by def tDepth
--       
--         RHS:  
--             tDepth (Leaf x)   
--             = 0                                                             by def tDepth
--             = LHS     
--           
--     I.H.
--         (tDepth . tMirror) u == tDepth u
--     
--     inductive case, t = (Node left x right):    
--         LHS:  
--             (tDepth . tMirror) (Node left x right)   
--             = tDepth (tMirror (Node left x right))                          by def .
--             = tDepth (Node (tMirror right) x (tMirror left))                by def tMirror
--             = 1 + max (tDepth (tMirror right)) (tDepth (tMirror left))      by def tDepth
--             = 1 + max ((tDepth . tMirror) right) ((tDepth . tMirror) left)  by function composition
--             = 1 + max (tDepth right) (tDepth left)                          by I.H.
--         RHS:
--             tDepth (Node left x right)
--             = 1 + max (tDepth left) (tDepth right)                          by def tDepth
--             = LHS                                                           by commutativity
 



-- pre-condition  : 
--     n >= 0
-- post-condition : 
--     trib n = correct tribonacci number at index n

trib :: Integral a => a -> a
trib 0 = 0
trib 1 = 0
trib 2 = 1
trib n = trib (n - 1) + trib (n - 2) + trib (n - 3)


trib2 :: Integral a => a -> a
trib2 n = trib2_ n 0 0 1

trib2_ :: Integral a => a -> a -> a -> a -> a
trib2_ n a b c 
    | n > 0 = trib2_ (n - 1) b c (a + b + c)
    | otherwise = a

-- proof of correctness for trib:
--
--    where:
--        T n = correct tribonacci number at index n
-- 
--     base case, n = 0:
--         LHS:
--             trib 0
--             = 0                                                             by def trib
--         RHS:
--             T 0
--             = 0                                                             by def tribonacci sequence
--             = LHS
--     base case, n = 1:
--         LHS:
--             trib 1
--             = 0                                                             by def trib
--         RHS:
--             T 1
--             = 0                                                             by def tribonacci sequence
--             = LHS
--     base case, n = 2:
--         LHS:
--             trib 2
--             = 1                                                             by def trib
--         RHS:
--             T 2
--             = 1                                                             by def tribonacci sequence
--             = LHS
-- 
--     I.H.
--         trib n == T n
--         for every n < k, for every k in natural numbers s.t. k >= 3
--
--     inductive case, n = k
--         where: 
--             k >= 3
--         LHS:
--             trib k
--             = trib (k - 1) + trib (k - 2) + trib (k - 3)                    by def trib
--             = T (k - 1) + T (k - 2) + T (k - 3)                             by I.H.
--         RHS:
--             T k
--             = T (k - 1) + T (k - 2)
--             + T (k - 3)                                                     by def tribonacci sequence
--             = LHS

-- proof of correctness for trib2, by proving trib2_:
--
--    where:
--        tribonacci sequence generator defined by T
--        T n = correct tribonacci number at index n
--
--    base case, n = 0:
--        LHS:
--            trib2_ 0 (T 0) (T 1) (T 2)
--            = T 0                                                            by def trib2_
--        RHS:
--            T 0
--            = LHS
--    base case, n = 1:
--        LHS:
--            trib2_ 1 (T 0) (T 1) (T 2)
--            trib2_ 0 (T 1) (T 2) ((T 0) + (T 1) + (T 2))
--            = T 1                                                            by def trib2_
--        RHS:
--            T 1
--            = LHS
--    base case, n = 2:
--        LHS:
--            trib2_ 2 (T 0) (T 1) (T 2)
--            = trib2_ 1 (T 1) (T 2) ((T 0) + (T 1) + (T 2))                   by def trib2_
--            = trib2_ 0 (T 2) 
--                       ((T 0) + (T 1) + (T 2)) 
--                       ((T 1) + (T 2) + ((T 0) + (T 1) + (T 2)) )            by def trib2_
--            = T 2                                                            by def trib2_
--        RHS:
--            T 2
--            = LHS
--
--    I.H.
--        (trib2_ k (T x) (T (x + 1)) (T (x + 2))) == T (k + x)
--     
--    inductive case, n = k + 1
--        LHS:
--            trib2_ (k + 1) (T 0) (T 1) (T 2)
--            = trib2_ k (T 1) (T 2) ((T 0) + (T 1) + (T 2))                   by def trib2_
--            = T (k + 1)                                                      by I.H.
--        RHS:
--            T (k + 1)
--            = LHS


-- int trib2_while(int n) {
-- 
--   int a, b, c, temp;
-- 
--   a = 0;
--   b = 0;
--   c = 1;
--   
--   while (n > 0) {
--       temp = c;
--       c = a + b + c;
--       a = b;
--       b = temp;
--   
--       n -= 1;
--   }
-- 
--   return a;
-- }
