import Control.Exception (assert)
import PPM2PGM

t1 = assert (luminace (255, 255, 255) == 255) "White pixel is white"
t2 = assert (luminace (  0,   0,   0) ==   0) "Black pixel is black"
t3 = assert (luminace ( 22,  14,   7) ==  15)

tests = [t1, t2]
