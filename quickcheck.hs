import Test.QuickCheck

g :: IO [Bool]
g = generate $ variant 4 $ vector 5

prop_reverse_len :: [Int] -> Bool
prop_reverse_len xs = length xs == (length $ reverse xs)

-- prop_reverse_head :: [Int] -> Property
prop_reverse_head xs =
  not (null xs) ==> last xs == (head $ reverse xs)
