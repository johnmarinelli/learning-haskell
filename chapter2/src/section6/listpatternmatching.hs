-- pattern matching on lists
-- lists have the [] ctor or the [x:xs] ctor

head' :: [a] -> a
head' [] = error "Can't call head on empty list"
head' (x:_) = x

empty' :: [a] -> Bool
empty' [] = True
empty' (x:_) = False

tail' :: [a] -> [a]
tail' [] = error "No tail"
tail' (_:xs) = xs

[] +++ lst2 = lst2
(x:xs) +++ lst2 = x:(xs +++ lst2)

{-
sorted [] = True
sorted [_] = True
sorted (x:y:zs) = x < y && sorted (y:zs)
-}

-- as patterns; haskell syntactic sugar
sorted [] = True
sorted [_] = True

-- bind to some value of the match, then bind to the inner
-- components of that value
-- wrap the pattern you want to give a name to in parens,
-- prepend it with the variable that will be used to refer to the pattern
-- and the @ symbol.
-- "(y:_) as 'r'"
sorted (x : r @(y:_) ) = x < y && sorted (r)

maxmin [x] = (x, x)
maxmin (x:xs) = ( if x > xs_max then x else xs_max
                , if x < xs_min then x else xs_min 
                ) where (xs_max, xs_min) = maxmin xs

{-
ifib :: Integer -> Maybe Integer
ifib n = if n < 0
         then Nothing
         else case n of
              0 -> Just 0
              1 -> Just 1
              n -> let Just f1 = ifib (n-1)
                       Just f2 = ifib (n-2)
                   in Just (f1 + f2)

binom _ 0 = 1
binom x x = 1
binom n k = (binom (n-1) (k-1)) + (binom (n-1) (k))
-}

-- guards:
-- boolean check that allows backtracking
ifib n | n < 0 = Nothing
ifib 0 = 0
ifib 1 = 1
ifib n | otherwise = let (Just f1, Just f2) = (ifib (n-1), ifib (n-2))
                     in Just (f1 + f2)

binom _ 0 = 1
binom x y | x == y = 1
binom x y | x < y  = 0
binom n k | otherwise = (binom (n-1) (k-1)) + (binom (n-1) k)
