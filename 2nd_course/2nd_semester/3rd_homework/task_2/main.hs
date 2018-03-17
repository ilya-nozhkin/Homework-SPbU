numbers179 :: [Integer]
numbers179 = 1:7:9:(concatMap (\x -> (map (x * 10 + ) (take 3 numbers179))) numbers179)

main :: IO ()
main = do
    putStr "Enter the number (n): " 
    line <- getLine

    let input = read line
    let output = take input numbers179

    putStrLn ("First n elements of the list of numbers with digits 1, 7, 9: " ++ show output)
