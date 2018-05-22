import Control.Monad.Zip

mzip3 a b c = mzipWith (\x (y, z) -> (x, y, z)) a (mzip b c)

findHill :: Ord b => [b] -> Maybe b
findHill (a:b:c:xs) = (\l -> if (null l) then Nothing else Just (head l))
    ((mzip3 (a:b:c:xs) (b:c:xs) (c:xs)) >>= 
        (\(x, y, z) -> if (y > x) && (y > z) then [y] else []))
findHill _ = Nothing
