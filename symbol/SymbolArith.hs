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

showArithParens0 (BinaryArith op a b) =
  "(" ++ showArithParens0 a ++ showOp op ++ showArithParens0 b ++ ")"
showArithParens0 e                    = showArith e

showArithParens (BinaryArith op a b) =
  showArithParens0 a ++ showOp op ++ showArithParens0 b
showArithParens e                    = showArith e

getSymbol (BinaryArith Mul s@(Symbol _) b) = Just (b, s)
getSymbol (BinaryArith Mul a s@(Symbol _)) = Just (a, s)
getSymbol s@(Symbol _)                     = Just (1, s)
getSymbol _                                = Nothing

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
moveParensRight a@(BinaryArith opA b@(BinaryArith opB c d) e)
  | precedence a == precedence b =
      Just $ BinaryArith opB c (BinaryArith opA d e)
  | otherwise        = Nothing
moveParensRight _ = Nothing

moveParensLeft a@(BinaryArith opA b c@(BinaryArith opC d e))
  | precedence a == precedence c =
      Just $ BinaryArith opC (BinaryArith opA b d) e
  | otherwise        = Nothing
moveParensLeft _ = Nothing

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

-- Change order of an arithmetic tree into a more "canonical" form
canonify (BinaryArith op a b@(Number _)) = BinaryArith op b a
canonify a@(BinaryArith _ (BinaryArith _ (Number _) _) _) =
  maybe a id $ moveParensRight a  
canonify e = e

canonifyChild (BinaryArith op b c) =
  (do
      b' <- canonifyMaybe b
      return $ BinaryArith op b' c)
  <|>
  (do
      c' <- canonifyMaybe c
      return $ BinaryArith op b c')
canonifyChild _ = Nothing

canonifyMaybe e
  | e /= e'   = Just e'
  | otherwise = Nothing
  where e' = canonify e

simplifyMaybe :: (Eq a, Floating a) => SymbolArith a -> Maybe (SymbolArith a)
simplifyMaybe (Symbol _) = Nothing
simplifyMaybe (Number _) = Nothing
simplifyMaybe (BinaryArith Sum (Number 0) s) = Just s
simplifyMaybe (BinaryArith Sum s (Number 0)) = Just s
simplifyMaybe (BinaryArith Neg (Number 0) s) = Just s
simplifyMaybe (BinaryArith Neg s (Number 0)) = Just s
simplifyMaybe (BinaryArith Mul (Number 1) s) = Just s
simplifyMaybe (BinaryArith Mul s (Number 1)) = Just s
simplifyMaybe (BinaryArith Mul (Number 0) _) = Just $ Number 0
simplifyMaybe (BinaryArith Mul _ (Number 0)) = Just $ Number 0
simplifyMaybe (BinaryArith Div s (Number 1)) = Just s
simplifyMaybe (BinaryArith Div (Number 0) _) = Just $ Number 0
simplifyMaybe (BinaryArith op (Number a) (Number b)) =
  Just . Number $ binaryFunc op a b
simplifyMaybe a@(BinaryArith _  (Number _) (BinaryArith _ (Number _) _)) =
  (moveParensLeft a >>= simplifyMaybe) <|> simplifyBinary a
simplifyMaybe a@(BinaryArith _ _ _) = simplifyBinary a
simplifyMaybe e = canonifyMaybe e

simplifyOnce a = maybe a id $ simplifyMaybe a
simplify a = case simplifyMaybe a of
  Just a' -> simplify a'
  Nothing -> a

simplifyBinary a@(BinaryArith _ _ _) =
  sameSymbolOp a <|> simplifyChild a <|> canonifyMaybe a
simplifyBinary _ = Nothing

simplifyChild :: (Eq a, Floating a) =>
  SymbolArith a -> Maybe (SymbolArith a)
simplifyChild (BinaryArith op b c) =
  (do
      b' <- simplifyMaybe b
      return $ BinaryArith op b' c)
  <|>
  (do
      c' <- simplifyMaybe c
      return $ BinaryArith op b c')
simplifyChild _ = Nothing
-- TODO: "x + 2.0 + 3.0*x"
