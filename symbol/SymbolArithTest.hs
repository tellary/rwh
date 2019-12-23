import Control.Exception (assert)
import SymbolArith

t1 = assert ((showArithParens $ 1 + 2 + 3) == "(1 + 2) + 3") "t1"
t2 = assert ((showArithParens . canonify $ 1 + 2 + 3) == "3 + (1 + 2)") "t2"

t3 = assert
     ((fmap showArithParens . moveParensRight $ 1 + 2 - 3)
      == Just "1 + (2 - 3)") "t3"
t4 = assert
     ((moveParensRight =<< (moveParensRight $ 1 + 2 + 3)) == Nothing)
     "`moveParensRight` doesn't work twice"

t5 = assert
     ((fmap showArithParens . moveParensLeft $ 1 + (2 - 3))
      == Just "(1 + 2) - 3") "t5"
t6 = assert
     ((moveParensLeft =<< (moveParensLeft $ 1 + 2 + 3)) == Nothing)
     "`moveParensLeft` doesn't work twice"

t7 = assert
     ((showArith . simplify $ sym "x" + 2 + 3*sym "x") == "2.0 + 4.0*x")
     ("simplify (x + 2 + 3x) works")

t8 = assert
     ((showArith <$> simplify $ sym "x" ** 2 + 2 + 3*sym "x" ** 2)
      == "2.0 + 4.0*x^2.0")
     ("simplify (x^2 + 2 + 3x^2) works")

t9 = assert
     ((showArith <$> simplify $ sin (sym "x") ** 2 + 2 + 3*sin (sym "x") ** 2)
      == "2.0 + 4.0*sin x^2.0")
     ("simplify (sin x^2 + 2 + sin x^2) works")

tests = [t1, t2, t3, t4, t5, t6, t7, t8, t9]
