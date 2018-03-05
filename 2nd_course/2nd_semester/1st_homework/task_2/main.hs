fibonacci :: Integer -> Integer
fibonacci 0 = 1
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

main :: IO()
main = do
    putStr "Enter the number (n): " 
    line <- getLine

    let input = read line 
    let output = fibonacci input

    putStrLn ("n'th fibonacci number is " ++ show output)
