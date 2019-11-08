module Prettifier(
  Doc, text, char,
  line, empty,
  indent, outdent,
  hcat, fsep, (<>), (</>), (<->),
  compact, width,
  fillChar,
  indentedChar, indented) where

data Doc = Text String |
           Char Char |
           Line |
           Union Doc Doc |
           Concat Doc Doc |
           Empty |
           Indent |
           Outdent deriving (Eq, Show)

text [] = Empty
text s = Text s

char = Char

line = Line

empty = Empty

indent = Indent
outdent = Outdent

hcat :: [Doc] -> Doc
hcat = fold (<>)
fsep ::  [Doc] -> Doc
fsep = fold (</>)
fold f = foldr f empty

Empty <> b = b
a <> Empty = a
a <> b = Concat a b
a </> b = a <> softline <> b
a <-> b = a <> (Char ' ' `Union` Empty) <> b

softline = group line

group a = flatten a `Union` a

flatten (a `Concat` b) = (flatten a) `Concat` (flatten b)
flatten Line = Char ' '
flatten (a `Union` _) = flatten a
flatten other = other

compact d = process [d]
  where process [] = []
        process (d:ds) =
          case d of
            Text s -> s ++ process ds
            Char c -> c:process ds
            Line -> '\n':process ds
            Empty -> process ds
            Indent -> process ds
            Outdent -> process ds
            a `Concat` b -> process (a:b:ds)
            a `Union` _ -> process (a:ds)

width width d = best 0 [d]
  where best _   [] = []
        best col (d:ds) =
          case d of
            Text s -> s ++ best (col + length s) ds
            Char c -> c:best (col + 1) ds
            Line -> '\n':best 0 ds
            Empty -> best col ds
            Indent -> best col ds
            Outdent -> best col ds
            a `Concat` b -> best col (a:b:ds)
            a `Union` b -> nicest width col
                           (best col (a:ds))
                           (best col (b:ds))

w `fits` _ | w < 0 = False
_ `fits` "" = True
_ `fits` ('\n':_) = True
w `fits` (_:cs) = (w - 1) `fits` cs

fillChar char width d = best 0 [d]
  where best col [] = replicate (charsLeft width col) char
        best col (d:ds) =
          case d of
            Text s -> s ++ best (col + length s) ds
            Char c -> c:best (col + 1) ds
            Line -> replicate (charsLeft width col) char ++
                    '\n':best 0 ds
            Empty -> best col ds
            Indent -> best col ds
            Outdent -> best col ds
            a `Concat` b -> best col (a:b:ds)
            a `Union` b -> nicest width col
                           (best col (a:ds))
                           (best col (b:ds))

nicest width col a b
  | charsLeft width col `fits` a = a
  | otherwise = b
charsLeft width col
  | charsLeft' < 0 = 0
  | otherwise = charsLeft'
  where least = min col width
        charsLeft' = width - least

indented = indentedChar ' '
indentedChar fillChar indent d = best 0 [d]
  where best _ [] = []
        best depth (d:ds) =
          case d of
            Text s -> s ++ best depth ds
            Char c -> c:best depth ds
            Line -> ('\n':replicate (indent*depth) fillChar) ++
                    best depth ds
            Empty -> best depth ds
            Indent -> best (depth + 1) ds
            Outdent -> best (depth - 1) ds
            a `Concat` b -> best depth (a:b:ds)
            _ `Union` b -> best depth (b:ds)
