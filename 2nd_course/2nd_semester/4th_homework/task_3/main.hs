import Data.Tree

--BFS
elems :: Tree a -> [a]
elems t = (concatMap (\(_, r) -> r) . 
           (\(a, b) -> a ++ [head b]) .
           span (\(c, _) -> not (null c)) . 
           iterate (\(c, r) -> (concatMap (subForest) c, (map (rootLabel) c))))
           ([t], [])
