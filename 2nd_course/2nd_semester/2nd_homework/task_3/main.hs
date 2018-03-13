sumOfDigits :: Integer -> Integer
sumOfDigits = sumOfDigits' 0 . abs where
    sumOfDigits' r 0 = r
    sumOfDigits' r n = sumOfDigits' (r + mod n 10) (div n 10)

main :: IO()
main = do
    putStr "Enter the number (n): " 
    line <- getLine

    let input = read line
    let output = sumOfDigits input

    putStrLn ("Sum of digits = " ++ show output)
