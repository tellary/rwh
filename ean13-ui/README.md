Barcode recognition written in Haskell and running in your browser
==================================================================

This Web UI showcases use of [Miso][miso]+[GHCJS][ghcjs]
on a barcode recognition example from
[Chapter 12][ch12] of the "Real World Haskell" book. The barcode recognition
code is located in the [`ch12/EAN13.hs`](../ch12/EAN13.hs). It differs
from the original source code in the book, because it was a "clean-room'ish"
re-implementation attempt, to check understanding of the chapter
by rewriting the example from scratch without peeking into the book much.

You'll also find that it uses [Netpbm format][npbm] parser from
[Chapter 10][ch10] of the book re-implemented in the
[`ch10/PPM.hs`](../ch10/PPM.hs) and [`ch10/PGM.hs`](../ch10/PGM.hs) files.

It also uses custom [parser combinators](../ch10/Parser.hs)
described in the book instead of some established library
like Parsec, Attoparsec etc., because this project serves
educational purpose.

I hope this exercise may serve as a tangible example and
help someone to dig through the "Real World Haskell" book.

[miso]:  https://haskell-miso.org/
[ghcjs]: https://github.com/ghcjs
[ch12]:  http://book.realworldhaskell.org/read/barcode-recognition.html
[ch10]:  http://book.realworldhaskell.org/read/code-case-study-parsing-a-binary-data-format.html
[npbm]:  http://netpbm.sourceforge.net/
