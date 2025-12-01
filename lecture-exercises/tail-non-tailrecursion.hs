-- recursive functions are either tail or nontail

-- tail recursion has inductive step that calls function directly on smaller "value" (tail)
-- recursive step is directly call on itself with smaller value

-- non tail recursion also calls value on tail, but they do some computation with the recursive call
-- recursive step has computation done on result from call on itself aswell

-- tail is more efficient, more computational advantage
-- as you need to unwind and rewind the recursion
-- non tail can usually be converted into a tail recursive function by adding extra arguments in the function

-- such as accumulators (which usually get bigger as variant becomes smaller)



