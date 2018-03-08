find :: Integer -> ([Integer] -> Integer)
find x = fst . headSecured . filter ((== x) . snd) . zip [1, 2..] where
    headSecured [] = (-1, -1)
    headSecured (e:es) = e

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

    putStrLn ("The first occurence of this number is in position (-1 means that number is not found): " ++ show output)
