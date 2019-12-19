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
precedence (UnaryArith _ _) = 9
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

showParens a = "(" ++ showArith a ++ ")"

showOp Mul = "*"
showOp Div = "/"
showOp Sum = " + "
showOp Neg = " - "
showOp Pow = "^"

showArith (Symbol s)            = show s
showArith (Number n)            = show n
showArith (UnaryArith op a)     = op ++ showArith a

showArith a@(BinaryArith op b c)
  | pa <= pb && pa <= pc = showArith  b ++ showOp op ++ showArith  c
  | pa <= pb             = showArith  b ++ showOp op ++ showParens c
  |             pa <= pc = showParens b ++ showOp op ++ showArith  c
  | otherwise            = showParens b ++ showOp op ++ showParens c
  where pa = precedence a
        pb = precedence b
        pc = precedence c
