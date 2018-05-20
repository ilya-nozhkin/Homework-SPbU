countEven1 = length . (filter even)
countEven2 = (foldr (+) 0) . (map $ (`mod` 2) . (+ 1))

countEven3 :: [Integer] -> Integer
countEven3 = foldr (\a b -> if even a then b + 1 else b) 0
