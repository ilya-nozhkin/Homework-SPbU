find :: Integer -> ([Integer] -> Maybe Integer)
find x = head' . filter ((== x) . snd) . zip [1, 2..] where
    head' [] = Nothing
    head' (e:es) = Just (fst e)

main :: IO()
main = do
    putStr "Enter the list of numbers divided by space: " 
    listLine <- getLine
    putStr "Enter the number contained in this list: "
    numberLine <- getLine

    let list = map (\x -> read x :: Integer) (words listLine)
    let number = read numberLine
    let output = find number list 

    putStrLn ("The first occurence of this number is in position: " ++ show output)
