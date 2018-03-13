mreverse :: [a] -> [a]
mreverse = mreverse' [] where
    mreverse' l [] = l
    mreverse' l (t:ts) = mreverse' (t:l) ts

main :: IO()
main = do
    putStr "Enter the number (n): " 
    line <- getLine

    let input = take (read line) [0,1..] 
    let output = mreverse input

    putStrLn ("List of n elements: " ++ show input)
    putStrLn ("Reversed: " ++ show output)
