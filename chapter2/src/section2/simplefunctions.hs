firstOrEmpty :: [[Char]] -> [Char]
firstOrEmpty lst = if null lst then "empty" else head lst

lst1 +++ lst2 = if null lst1
                then lst2
                else head lst1 : (tail lst1 +++ lst2)

reverse2 lst = if null lst 
               then []
               else reverse2 (tail lst) +++ [head lst]

-- local binding:
-- let introduces bindings before main expression
-- must end with 'in'
-- where binds after main expression
maxmin lst = let h = head lst
             in if null (tail lst) -- base case
                then (h, h)        -- return last element of list in pair
                else ( if h > t_max then h else t_max
                     , if h < t_min then h else t_min ) -- return (max, min)
                     where t = maxmin (tail lst) -- recursive call
                           t_max = fst t
                           t_min = snd t

{-
example for maxmin:
maxmin [3, 4, 6, 1]
  h = 3, maxmin [4, 6, 1]
    h = 4, maxmin [6, 1]
      h = 6, maxmin [1]
             => (1, 1)
      do the else statement with 6 and (1, 1)
    do the else statement with 4 and (6, 1)
  do the else statement with 3 and (6, 1)
return (6,1)
-}
