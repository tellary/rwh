module SymbolArith where

import Control.Applicative ((<|>))

data Op = Sum | Neg | Mul | Div | Pow
  deriving (Eq, Ord, Show)

binaryFunc Sum = (+)
binaryFunc Neg = (-)
binaryFunc Mul = (*)
binaryFunc Div = (/)
binaryFunc Pow = (**)

commutative Sum = True
commutative Neg = False
commutative Mul = True
commutative Div = False
commutative Pow = False

data SymbolArith a =
    Number a
  | Symbol String
  | UnaryArith String (SymbolArith a)
  | BinaryArith Op (SymbolArith a) (SymbolArith a)
  deriving (Eq, Ord, Show)

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

getMultiple (BinaryArith Mul a s) = Just (a, s)
getMultiple s                     = Just (1, s)

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
  (bn, bs) <- getMultiple b
  (cn, cs) <- getMultiple c
  if (bs == cs)
    then Just $ BinaryArith Mul (BinaryArith op bn cn) bs
    else Nothing
sameSymbolMul b c = do
  (bn, bs) <- getMultiple b
  (cn, cs) <- getMultiple c
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

moveNumberLeft (BinaryArith op a b@(Number _))
  | commutative op = Just $ BinaryArith op b a
  | otherwise      = Nothing
moveNumberLeft _   = Nothing

moveNumberLeftOfParens a@(BinaryArith _ (BinaryArith _ (Number _) _) _) =
  moveParensRight a
moveNumberLeftOfParens _ = Nothing

orderBinary (BinaryArith op a b)
  | commutative op && a > b = Just $ BinaryArith op b a
  | otherwise               = Nothing
orderBinary _ = Nothing

-- Change order of an arithmetic tree into a more "canonical" form
canonifyMaybe e = moveNumberLeft e <|> moveNumberLeftOfParens e
  <|> orderBinary e

canonify e = case canonifyMaybe e of
  Just e' -> e'
  Nothing -> e

canonifyChild (BinaryArith op b c) =
  (do
      b' <- canonifyMaybe b
      return $ BinaryArith op b' c)
  <|>
  (do
      c' <- canonifyMaybe c
      return $ BinaryArith op b c')
canonifyChild _ = Nothing

simplifyMaybe :: (Eq a, Ord a, Floating a) =>
  SymbolArith a -> Maybe (SymbolArith a)
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
simplifyMaybe a@(BinaryArith _ _ _) = simplifyBinary a
simplifyMaybe (UnaryArith fn e) = UnaryArith fn <$> simplifyMaybe e

simplifyOnce a = maybe a id $ simplifyMaybe a
simplify a = case simplifyMaybe a of
  Just a' -> simplify a'
  Nothing -> a

simplifyBinary a@(BinaryArith _ _ _) =
  sameSymbolOp a <|> simplifyChild a <|> tryParensLeft a <|> canonifyMaybe a
simplifyBinary _ = Nothing

simplifyChild :: (Eq a, Ord a, Floating a) =>
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

tryParensLeft e@(BinaryArith _ a (BinaryArith _ b _))
  -- This guard is necessary to avoid recursion
  | a <= b    = moveParensLeft e >>= simplifyMaybe
  | otherwise = Nothing
tryParensLeft _                     = Nothing
