signs :: [Integer]
signs = 1:(map (*(-1)) signs)

numbersWithAlternatingSigns :: [Integer]
numbersWithAlternatingSigns = zipWith (*) signs [1,2..]
