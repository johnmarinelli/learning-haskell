data TimeMachine = TimeMachine String Integer Bool Float
                   deriving Show

{-Every year a time comes when time machines are sold with a big discount to encourage potential
buyers. Write a function that given a list of time machines, decreases their price by some
percentage. Use the TimeMachine data type you defined in the previous set of exercises. -}

discount :: Float -> TimeMachine -> TimeMachine
discount pct tm = case tm of
                     TimeMachine name id somebool price -> (TimeMachine name id somebool (price - (price * pct)))

discountMachines :: [TimeMachine] -> Float -> [TimeMachine]
discountMachines lst pct = map (discount pct) lst 
