import Data.List

insertIntoSorted :: Integer -> [Integer] -> [Integer]
insertIntoSorted e l = insertIntoSorted' e l where
    insertIntoSorted' e [] = [e]
    insertIntoSorted' e (t:ts)
      | t < e = t:(insertIntoSorted' e ts)
      | otherwise = e:t:ts

getCommand :: IO [Integer]
getCommand = getLine >>= 
    return . (map (\w -> read w :: Integer)) . words

interpret :: [Integer] -> [Integer] -> IO ([Integer])
interpret l [0] = return l 
interpret l (1:v:[]) = getCommand >>= (interpret (insertIntoSorted v l))
interpret l (2:v:[]) = getCommand >>= (interpret (delete v l))
interpret l [3] = (putStrLn . show) l >> getCommand >>= interpret l
interpret l _ = putStrLn "Unknown operation" >> getCommand >>= interpret l

main :: IO ()
main = getCommand >>= interpret [] >> return ()
