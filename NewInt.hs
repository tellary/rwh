newtype NewInt = N Int deriving (Show, Eq)

i (N a) = a
newSum (N a) (N b) = N (a + b)

instance Num NewInt where
  N a + N b = N (a + b)
  N a * N b = N (a * b)
  N a - N b = N (a - b)
  abs (N a) = N (abs a)
  signum (N a) = N (signum a)
  fromInteger a = N (fromInteger a)
    
