{-# LANGUAGE RecordWildCards #-}
data TimeMachineRecord = TimeMachineRecord { name  ::  String
                                           , id    ::  Integer 
                                           , works :: Bool 
                                           , price :: Float } deriving Show

{-Every year a time comes when time machines are sold with a big discount to encourage potential
buyers. Write a function that given a list of time machines, decreases their price by some
percentage. Use the TimeMachine data type you defined in the previous set of exercises. -}
{-2.7: rewrite using data records-}
discount :: Float -> TimeMachineRecord -> TimeMachineRecord
discount pct tmr@(TimeMachineRecord { .. }) = tmr { price = (price - (price * pct)) }

discountMachines :: [TimeMachineRecord] -> Float -> [TimeMachineRecord]
discountMachines lst pct = map (discount pct) lst 
