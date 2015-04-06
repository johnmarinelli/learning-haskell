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
unzip' lst | null (tail lst) = ([fst (head lst)], [snd (head lst)])
unzip' lst =  ( fst (head lst) : fst (unzip' (tail lst)), 
                snd (head lst) : snd (unzip' (tail lst)) 
              )
