-- ackermann function
-- if m = 0, n+1
-- if m > 0 && n = 0, A(m-1, 1)
-- if m > 0 && n > 0, A(m-1, A(m, n-1))

ack 0 n | n >= 0 = n + 1
ack m 0 | m > 0  = ack (m-1) 1
ack m n | otherwise = ack (m-1) (ack m (n-1))

-- unzip
-- input: pairs
-- output: two lists; one with first components and other with snd components

unzip' :: [(a,a)] -> ([a], [a])

-- base case.  when lst only has one member, return a pair containing a single element
-- array of the first and second items of the element in lst.
unzip' lst | null (tail lst) = ([fst (head lst)], [snd (head lst)])

-- recursive step.  concat the first and second items in the head of lst
-- to a recursive call to unzip'.
unzip' lst =  ( fst (head lst) : fst (unzip' (tail lst)), 
                snd (head lst) : snd (unzip' (tail lst)) 
              )
