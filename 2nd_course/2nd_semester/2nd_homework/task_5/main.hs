sumLists :: [Integer] -> [Integer] -> [Integer] -> [Integer]
sumLists [] [] [] = []
sumLists a b c = ((head' a) + (head' b) + (head' c)):(sumLists (tail' a) (tail' b) (tail' c)) where
    head' [] = 0
    head' (e:es) = e
    tail' [] = []
    tail' (e:es) = es

split :: String -> [String]
split s = split' s "" [] where
    split' "" "" r = reverse r
    split' "" t r = reverse ((reverse t):r)
    split' (' ':cs) "" r = split' cs "" r
    split' (' ':cs) t r = split' cs "" ((reverse t):r)
    split' (c:cs) t r = split' cs (c:t) r 

main :: IO()
main = do
    putStr "Enter the first list of numbers divided by space: " 
    listLine1 <- getLine
    putStr "Enter the second list of numbers divided by space: " 
    listLine2 <- getLine
    putStr "Enter the third list of numbers divided by space: " 
    listLine3 <- getLine

    let list1 = map (\x -> read x :: Integer) (split listLine1)
    let list2 = map (\x -> read x :: Integer) (split listLine2)
    let list3 = map (\x -> read x :: Integer) (split listLine3)
    let output = sumLists list1 list2 list3

    putStrLn ("List of sum of elements of those lists: " ++ show output)
