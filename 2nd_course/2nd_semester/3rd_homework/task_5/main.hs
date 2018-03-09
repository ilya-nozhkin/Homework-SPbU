fibonacci :: Integer -> Integer
fibonacci n 
  | abs n >= 2 = fibonacci' (abs n - 2) 0 1 (if n >= 0 then (+) else (-))
  | otherwise = abs n
    where
        fibonacci' n p c f
          | n == 0 = f p c
          | otherwise = fibonacci' (n - 1) c (f p c) (f)

main :: IO ()
main = do
    putStr "Enter the number (n): "
    line <- getLine

    let input = read line
    let output = fibonacci input

    putStrLn ("n'th fibonacci number is: " ++ show output)
