powersOfTwo :: Integer -> [Integer]
powersOfTwo n = [2 ^ i | i <- [0..n]]

main :: IO()
main = do
    putStr "Enter the number (n): " 
    line <- getLine

    let input = read line
    let output = powersOfTwo input

    putStrLn ("List of powers of two from 0'th to n'th: " ++ show output)
