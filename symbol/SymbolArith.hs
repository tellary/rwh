module SymbolArith where

import Control.Applicative ((<|>))

data Op = Sum | Neg | Mul | Div | Pow
  deriving (Eq, Show)

binaryFunc Sum = (+)
binaryFunc Neg = (-)
binaryFunc Mul = (*)
binaryFunc Div = (/)
binaryFunc Pow = (**)

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

sym s = Symbol s

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

getSymbol (BinaryArith Mul s@(Symbol _) b) = Just (b, s)
getSymbol (BinaryArith Mul a s@(Symbol _)) = Just (a, s)
getSymbol s@(Symbol _)                     = Just (Number 1, s)
getSymbol  _                             = Nothing

samePrecedence a@(BinaryArith _ b c) =
  precedence a == precedence b && precedence a == precedence c
samePrecedence _ = False

{- |
(1 + 2x) + 3x == 1 + (2x + 3x)

BinaryArith Sum
  (BinaryArith Sum
     (Number 1)
     (BinaryArith Mul
       (Number 2) (Symbol "x")
     )
  )
  (BinaryArith Mul (Number 3) (Symbol "x"))

BinaryArith Sum
  (Number 1)
  (BinaryArith Sum
     (BinaryArith Mul
       (Number 2) (Symbol "x")
     )
     (BinaryArith Mul (Number 3) (Symbol "x"))
  )
-}
swapTrinary a@(BinaryArith opA b@(BinaryArith opB c d) e)
  | precedence a == precedence b =
      Just $ BinaryArith opB c (BinaryArith opA d e)
  | otherwise        = Nothing
swapTrinary _ = Nothing

swapBinary (BinaryArith op a b) = Just $ BinaryArith op b a
swapBinary  _ = Nothing

sameSymbolSum op b c = do
  (bn, bs) <- getSymbol b
  (cn, cs) <- getSymbol c
  if (bs == cs)
    then Just $ BinaryArith Mul (BinaryArith op bn cn) bs
    else Nothing
sameSymbolMul b c = do
  (bn, bs) <- getSymbol b
  (cn, cs) <- getSymbol c
  if (bs == cs)
    then Just
         $ BinaryArith Mul
           (BinaryArith Mul bn cn)
           (BinaryArith Pow bs 2)
    else Nothing
sameSymbolOp (BinaryArith Sum b c) = sameSymbolSum Sum b c
sameSymbolOp (BinaryArith Neg b c) = sameSymbolSum Neg b c
sameSymbolOp (BinaryArith Mul b c) = sameSymbolMul     b c
sameSymbolOp _ = Nothing

simplify s@(Symbol _) = s
simplify n@(Number _) = n
simplify (BinaryArith Sum (Number 0) s) = s
simplify (BinaryArith Sum s (Number 0)) = s
simplify (BinaryArith Neg (Number 0) s) = s
simplify (BinaryArith Neg s (Number 0)) = s
simplify (BinaryArith Mul (Number 1) s) = s
simplify (BinaryArith Mul s (Number 1)) = s
simplify (BinaryArith Mul (Number 0) _) = Number 0
simplify (BinaryArith Mul _ (Number 0)) = Number 0
simplify (BinaryArith Div s (Number 1)) = s
simplify (BinaryArith Div (Number 0) _) = Number 0
simplify (BinaryArith op (Number a) (Number b)) = Number $ binaryFunc op a b
simplify a@(BinaryArith op b c) = r
  where Just r = sameSymbolOp a
                 <|> (do
                         a' <- swapTrinary a
                         return $ simplify a')
                 <|> Just (BinaryArith op (simplify b) (simplify c))
simplify e = e

-- TODO: "x + 2.0 + 3.0*x"
