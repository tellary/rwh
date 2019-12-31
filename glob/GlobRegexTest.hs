import Control.Exception (assert)
import GlobRegex
import Logger

t1 = assert
     ((runLogger $ matchesGlob False "f!!.c" "f[!][!].c")
      == (Right True, ["Escape .", "Character class", "Character class"]))
     "t1"

t2 = assert
     ((runLogger $ matchesGlob False "fa.c" "f[!!].c")
      == (Right True, ["Escape .", "Negative character class"]))
     "t2"

t3 = assert
     ((runLogger $ matchesGlob False "f!.c" "f[!!].c")
      == (Right False, ["Escape .", "Negative character class"]))
     "t3"

t4 = assert
     ((runLogger $ globToRegex "fo[.c")
      == (Left GlobUnterminatedClass,
          ["Escape .", "Character class"]))
     "t4"

t5 = assert
     ((runLogger $ globToRegex "fo[!.c")
      == (Left GlobUnterminatedNegativeClass,
          ["Escape .", "Negative character class"]))
     "t5"

t6 = assert
     ((runLogger $ matchesGlob False "fo].c" "fo].c")
      == (Right True, ["Escape .","Escape ]"]))
     "t6"

t7 = assert
     ((runLogger $ matchesGlob False "foo[" "foo[")
      == (Left GlobUnterminatedClass, []))
     "GlobUnterminatedClass on open"

t8 = assert
     ((runLogger $ matchesGlob False "foobar" "foo[]bar")
      == (Left GlobEmptyClass, []))
     "GlobEmptyClass"

t9 = assert
     ((runLogger $ matchesGlob False "foo.c" "foo.[!C]")
      == (Right True, ["Negative character class","Escape ."]))
     "Negative class"

t10 = assert
     ((runLogger $ matchesGlob True "foo.c" "foo.[!C]")
      == (Right False, ["Negative character class","Escape ."]))
     "Negative class case insensitive"

t11 = assert
     ((runLogger $ matchesGlob True "foo.c" "foo.[!")
      == (Left GlobUnterminatedNegativeClass, ["Escape ."]))
     "GlobUnterminatedNegativeClass on open"

tests = [t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11]

main = mapM_ putStrLn tests


