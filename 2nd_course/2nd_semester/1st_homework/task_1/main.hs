factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

main :: IO()
main = do
    putStr "Enter the number: " 
    line <- getLine

    let input = read line 
    let output = factorial input

    putStrLn ("factorial = " ++ show output)
