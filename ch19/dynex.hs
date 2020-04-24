import Control.Exception
import Data.Dynamic

data SqlError
  = SqlError
  { seState :: String
  , seNativeError :: Int
  , seErrorMsg :: String
  } deriving (Eq, Show, Typeable)

instance Exception SqlError

-- Throw and catch `SqlError` as `Dynamic`
throwCatchSqlError =
  (evaluate . throw . toDyn $ SqlError "state" 2 "error")
  `catch` \e -> return $ "Exception caught: " ++ show (e :: Dynamic)

