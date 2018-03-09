sumLists :: [Integer] -> [Integer] -> [Integer] -> [Integer]
sumLists [] [] [] = []
sumLists a b c = ((head' a) + (head' b) + (head' c)):(sumLists (tail' a) (tail' b) (tail' c)) where
    head' [] = 0
    head' (e:es) = e
    tail' [] = []
    tail' (e:es) = es

main :: IO()
main = do
    putStr "Enter the first list of numbers divided by space: " 
    listLine1 <- getLine
    putStr "Enter the second list of numbers divided by space: " 
    listLine2 <- getLine
    putStr "Enter the third list of numbers divided by space: " 
    listLine3 <- getLine

    let list1 = map (\x -> read x :: Integer) (words listLine1)
    let list2 = map (\x -> read x :: Integer) (words listLine2)
    let list3 = map (\x -> read x :: Integer) (words listLine3)
    let output = sumLists list1 list2 list3

    putStrLn ("List of sum of elements of those lists: " ++ show output)
