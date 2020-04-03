import Control.Monad.State
import Control.Monad.State.Class
import System.Random

getRandom :: (Random a, RandomGen g, MonadState g m) => m a
getRandom = do
  g <- get
  let (a, g') = random g
  put g'
  return a

getTwoRandoms :: (Random a, RandomGen g, MonadState g m) => m (a, a)
getTwoRandoms = do
  r1 <- getRandom
  r2 <- getRandom
  return (r1, r2)

seedTwoRandoms :: Random a => Int -> (a, a)
seedTwoRandoms s = evalState getTwoRandoms (mkStdGen s)
