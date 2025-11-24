data Q a = Q [a] [a] deriving (Eq, Show) --invariant: front is only empty when back is also empty

fixQ :: Q a -> Q a
fixQ (Q [] back) = Q (reverse back) []
fixQ q = q

empty :: Q a
empty = Q [] []

isEmpty :: Q a -> Bool
isEmpty (Q front back) = null front && null back

add :: a -> Q a -> Q a
add x (Q front back) = fixQ (Q front (x:back))

remove :: Q a -> Q a
remove (Q (x : front) back) = fixQ (Q front back)

front :: Q a -> a
front (Q (x : front) back) = x