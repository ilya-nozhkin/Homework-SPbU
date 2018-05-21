pairMultiplications n = [1..n] >>= (\i -> [1..n] >>= return . (*i))
