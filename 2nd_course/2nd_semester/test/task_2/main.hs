ones :: Int -> [Int]
ones l = (replicate l 1) ++ [0,0..]

matrix :: Int -> [[Int]]
matrix 0 = []
matrix n = matrix' n (n - 1) ([replicate n n]) where
    matrix' n 0 m = m
    matrix' n l (r:m) = matrix' n (l - 1) ((zipWith (-) r (ones l)):r:m)
