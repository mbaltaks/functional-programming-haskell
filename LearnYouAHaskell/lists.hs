myfilter :: (t -> Bool) -> [t] -> [t]
myfilter condition [] = []
myfilter condition (first:remainder)
    | condition first = first : myfilter condition remainder
    | otherwise = myfilter condition remainder
