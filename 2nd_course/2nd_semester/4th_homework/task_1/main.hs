countEven1 = foldr (\_ a -> a + 1) 0 . filter even
countEven2 = foldr (+) 0 . map $ (`mod` 2) . (+ 1)
countEven3 = 
