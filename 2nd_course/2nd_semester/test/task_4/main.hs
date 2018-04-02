supermap :: [a] -> (a ->[b]) -> [b]
supermap [] f = []
supermap (e:es) f = (f e) ++ supermap es f
