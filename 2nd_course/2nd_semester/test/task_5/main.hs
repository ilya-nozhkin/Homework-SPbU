checkList :: (a -> Bool) -> [a] -> Bool
checkList p [] = True
checkList p (e:es) = if p e then checkList p es else False
