import Control.Monad (mplus, mzero)

lookupM k [] = mzero
lookupM k ((x, y):xs)
  | k == x    = return y `mplus` lookupM k xs
  | otherwise = lookupM k xs

assoc = [(1, "one"), (1, "um"), (2, "two"), (3, "three"), (3, "три")]

-- λ> lookupM 1 assoc :: [String]
-- ["one","um"]
-- λ> lookupM 1 assoc :: Maybe String
-- Just "one"
-- λ> lookupM 2 assoc :: [String]
-- ["two"]
-- λ> lookupM 2 assoc :: IO String
-- "two"
-- λ> lookupM 2 assoc :: Maybe String
-- Just "two"
-- λ> lookupM 3 assoc :: [String]
-- ["three","\1090\1088\1080"]
