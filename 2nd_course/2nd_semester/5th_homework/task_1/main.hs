decompositions 1 = [[1]]
decompositions n = 
    (concatMap 
        (\i -> (map (i:) . (take i) . decompositions) (n - i)) 
        [1..(n-1)]) 
    ++ [[n]]

prettify = (\l -> foldr (\n s -> n ++ "+" ++ s) (head l) (tail l)) . (map show)

main = putStr "Enter a number: " >> getLine >>=
    (mapM_ (putStrLn . prettify)) . decompositions . (\s -> read s :: Int)
