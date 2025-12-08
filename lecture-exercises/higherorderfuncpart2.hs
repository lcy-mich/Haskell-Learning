-- function composition, do something, do something else

-- (f.g)x = f (g x)

-- f.g x is not the same as f (g x)
-- not all pairs can be composed, output type of g must be input type of f
-- codomain of g must be subset of domain of f

-- (.) :: (b -> c) -> (a -> b) -> (a -> c)

-- composition is associative f.(g.h) = (f.g).h

-- forward composition >.>

-- order of composition is significant
-- (f.g) mean apply g then apply f
-- can define an operator that applies functions in opposite order
(>.>) :: (a -> b) -> (b -> c) -> (a-> c)
g >.> f = f.g

-- application operator $
-- f e
-- f $ e
-- means same things

-- flipV (flipH (rotate horse))
-- flipV $ flipH $ rotate horse

-- can use as function
-- zipWith ($) [sum, product] [[1,2],[3,4]]


-- let id be the polymorphic identity function
-- id x = x
-- explain behaviour of (id.f) (f.id) id f

-- they all return the output of the function f

-- takes list of functions, into a single function type, give the type
-- composeList :: [(b->b)] -> b