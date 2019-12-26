import DList
import Test.QuickCheck

prop_backAndForth :: [Int] -> Bool
prop_backAndForth l = (toList . fromList $ l) == l

prop_append :: [Int] -> [Int] -> Bool
prop_append a b = (toList $ append (fromList a) (fromList b)) == a ++ b

tests = do
  t1' <- fmap (const "prop_backAndForth") $ quickCheck prop_backAndForth 
  t2' <- fmap (const "prop_append") $ quickCheck prop_append
  return [t1', t2']
