{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
import Prelude hiding ((++), length)
-- to prove function is true, do induction
-- show base case holds
--          sum (doubleAll []) = 2*sum []
-- and show inductive case holds
--          sum (doubleAll x:xs) = 2*x + 2*sum xs
-- base case:

-- LHS:
-- sum (doubleAll [])
-- sum []                                       by def doubleAll
-- =0 
-- 
-- RHS:
-- 2* sum []                                    by def sum
-- = 2*0                                
-- = 0                                          by arithmetic
-- = LHS
--
-- induction case:
-- LHS:
-- sum (doubleAll x:xs)
-- = sum ( 2*x : (doubleAll xs))                by def doubleAll
-- = 2*x + sum (doubleAll xs)                   by def sum
-- 
-- RHS:
-- 2* sum (x:xs)
-- = 2*(x + sum xs)                             by def sum
-- = 2*x + 2*sum xs                             by arithmetic
-- = LHS                                        by inductive hypothesis
-- 
-- 
-- 
-- 
-- 
length :: [a] -> Integer
length [] = 0
length (x:xs) = 1 + length xs

(++) :: [a] -> [a] -> [a]
[] ++ zs = zs
(w:ws) ++ zs = w : (ws ++ zs)

-- length (xs ++ ys) = length xs + length ys
--quickcheck
prop_lengthPlusPlus ::  [a] -> [a] -> Bool
prop_lengthPlusPlus ys xs = length(xs ++ ys) == length xs + length ys

-- base case
-- show length ([] ++ ys) = length [] + length ys
-- inductive case
-- show length ((x:xs) ++ ys) = length (x:xs) + length ys
-- ih
-- length (xs ++ ys) = length xs + length ys
-- 
-- base case
-- LHS:
-- length ([] ++ ys)
-- = length ys                                      by def ++
-- 
-- RHS:
-- length [] + length ys
-- = 0 + length ys                                  by def length
-- = length ys                                      by arithmetic
-- = LHS
-- 
-- inductive case
-- 
-- LHS:
-- length ((x:xs) ++ ys)
-- = length x:(xs ++ ys)                            by def ++
-- = 1 + length (xs + ys)                           by def length
-- 
-- RHS:
-- length (x:xs) + length ys
-- = 1+ length xs + length ys                       by def length
-- = LHS                                            by IH