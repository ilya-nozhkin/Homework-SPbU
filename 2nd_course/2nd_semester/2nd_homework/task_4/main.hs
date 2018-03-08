find :: Integer -> ([Integer] -> Maybe Integer)
find x = head' . filter ((== x) . snd) . zip [1, 2..] where
    head' [] = Nothing
    head' (e:es) = Just (fst e)

split :: String -> [String]
split s = split' s "" [] where
    split' "" "" r = reverse r
    split' "" t r = reverse ((reverse t):r)
    split' (' ':cs) "" r = split' cs "" r
    split' (' ':cs) t r = split' cs "" ((reverse t):r)
    split' (c:cs) t r = split' cs (c:t) r 

main :: IO()
main = do
    putStr "Enter the list of numbers divided by space: " 
    listLine <- getLine
    putStr "Enter the number contained in this list: "
    numberLine <- getLine

    let list = map (\x -> read x :: Integer) (split listLine)
    let number = read numberLine
    let output = find number list 

    putStrLn ("The first occurence of this number is in position: " ++ show output)
