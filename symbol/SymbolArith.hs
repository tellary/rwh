module SymbolArith where

data Op = Sum | Neg | Mul | Div | Pow
  deriving (Eq, Show)

data SymbolArith a =
    Symbol String
  | BinaryArith Op (SymbolArith a) (SymbolArith a)
  | UnaryArith String (SymbolArith a)
  | Number a
  deriving (Eq, Show)

precedence (Symbol     _  ) = 9
precedence (Number     _  ) = 9
precedence (UnaryArith _ _) = 8
precedence (BinaryArith Pow _ _) = 7
precedence (BinaryArith Mul _ _) = 6
precedence (BinaryArith Div _ _) = 6
precedence (BinaryArith Sum _ _) = 5
precedence (BinaryArith Neg _ _) = 5

instance Num a => Num (SymbolArith a) where
  a + b    = BinaryArith Sum a b
  a - b    = BinaryArith Neg a b
  a * b    = BinaryArith Mul a b
  abs a    = UnaryArith "abs"    a
  signum a = UnaryArith "signum" a
  fromInteger i = Number (fromInteger i)

instance Fractional a => Fractional (SymbolArith a) where
  a / b          = BinaryArith Div a b
  fromRational r = Number (fromRational r)

instance Fractional a => Floating (SymbolArith a) where
  pi     = Symbol "pi"
  exp    = UnaryArith "exp"
  log    = UnaryArith "log"
  sqrt   = UnaryArith "sqrt"
  a ** b = BinaryArith Pow a b
  sin    = UnaryArith "sin"
  cos    = UnaryArith "cos"
  tan    = UnaryArith "tan"
  asin   = UnaryArith "asin"
  acos   = UnaryArith "acos"
  atan   = UnaryArith "atan"
  sinh   = UnaryArith "sinh"
  cosh   = UnaryArith "cosh"
  tanh   = UnaryArith "tanh"
  asinh  = UnaryArith "asinh"
  acosh  = UnaryArith "acosh"
  atanh  = UnaryArith "atanh"

showParens a = "(" ++ showArith a ++ ")"

showOp Mul = "*"
showOp Div = "/"
showOp Sum = " + "
showOp Neg = " - "
showOp Pow = "^"

showArith (Symbol s) = s
showArith (Number n) = show n

showArith u@(UnaryArith op a)
  | pu <= pa  = op ++ " " ++ showArith  a
  | otherwise = op ++ " " ++ showParens a
  where pu = precedence u
        pa = precedence a

showArith a@(BinaryArith op b c)
  | pa <= pb && pa <= pc = showArith  b ++ showOp op ++ showArith  c
  | pa <= pb             = showArith  b ++ showOp op ++ showParens c
  |             pa <= pc = showParens b ++ showOp op ++ showArith  c
  | otherwise            = showParens b ++ showOp op ++ showParens c
  where pa = precedence a
        pb = precedence b
        pc = precedence c
