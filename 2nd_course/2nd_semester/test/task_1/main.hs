signs :: [Integer]
signs = 1:(map (*(-1)) signs)

numbersWithAlternatingSigns = zipWith (*) signs [1,2..]
