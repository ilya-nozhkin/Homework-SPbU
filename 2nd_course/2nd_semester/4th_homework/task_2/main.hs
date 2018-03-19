insertIntoSorted :: Integer -> [Integer] -> [Integer]
insertIntoSorted e l = insertIntoSorted' e l where
    insertIntoSorted' e [] = [e]
    insertIntoSorted' e (t:ts)
      | t < e = t:(insertIntoSorted' e ts)
      | otherwise = e:t:ts

getCommand :: IO [Integer]
getCommand = getLine >>= 
    return . (map (\w -> read w :: Integer)) . words

interpret :: [Integer] -> [Integer] -> [Integer]
interpret l [0] = l 
interpret l (1:v:[]) = getCommand >>= (interpret (insertIntoSorted v l))

main = getCommand >>= return . (interpret [])
