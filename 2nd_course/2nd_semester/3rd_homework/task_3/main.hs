import Data.List
import Data.Ord

findMaxSum :: [Integer] -> Integer
findMaxSum l = (fst . (maximumBy (comparing snd)) . (zip [1, 2..]) . tail) (zipWith (+) (0:l) l)

main :: IO ()
main = do
    putStr "Enter the list of numbers divided by space: " 
    line <- getLine

    let list = map (\x -> read x :: Integer) (words line)
    let output = findMaxSum list 

    putStrLn ("Position of the pair of elements with maximum sum is: " ++ show output)
