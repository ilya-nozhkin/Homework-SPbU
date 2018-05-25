import Data.Tree

--BFS
elems :: Tree a -> [a]
elems t = (concatMap (\(_, r) -> r) . 
           takeWhile (\(c, r) -> (not (null c)) || (not (null r))) . 
           iterate (\(c, r) -> (concatMap (subForest) c, (map (rootLabel) c))))
           ([t], [])
