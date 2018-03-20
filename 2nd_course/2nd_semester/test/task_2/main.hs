ones :: Int -> Int -> [Int]
ones l n = take n ((take l [1,1..]) ++ [0,0..])

matrix :: Int -> [[Int]]
matrix n = matrix' n (n - 1) [take n [n,n..]] where
    matrix' n 0 m = m
    matrix' n l (r:m) = matrix' n (l - 1) ((zipWith (-) r (ones l n)):r:m)
