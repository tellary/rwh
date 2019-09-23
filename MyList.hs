import Data.List


myLength xs = loop 0 xs
  where loop n [] = n
        loop n (x:xs) = loop (n + 1) xs

myNull [] = True
myNull _ = False

emptyError = error "Empty list" 

myHead [] = emptyError
myHead (x:xs) = x

myTail [] = emptyError
myTail (x:xs) = xs

myLast [] = emptyError
myLast [x] = x
myLast (x:xs) = myLast xs

myInit [] = emptyError
myInit [x] = []
myInit (x:xs) = x:(myInit xs)

[] `myAppend` l = l
(x:xs) `myAppend` ys = x:(xs `myAppend` ys)

myAppend2 xs ys = foldr copy ys xs
  where copy x r = x:r

myConcat [] = []
myConcat (xs:xss) = xs `myAppend` (myConcat xss)

myConcat2 :: [[a]] -> [a]
myConcat2 = foldr myAppend []

myReverse [] = []
myReverse xs = loop [] xs
  where loop r [] = r
        loop r (x:xs) = loop (x:r) xs

myReverse2 [] = []
myReverse2 xs = foldl' (flip (:)) [] xs

myAnd xs = foldl' (&&) True xs

myOr xs = foldl' (||) False xs

myAll p = myAnd . map p

myAny p = myOr . map p

myTake _ [] = []
myTake n (x:xs)
  | n <= 0 = []
  | otherwise = x:(myTake (n - 1) xs)
  
myDrop _ [] = []
myDrop n (x:xs)
  | n <= 0 = x:xs
  | otherwise = myDrop (n - 1) xs

myTakeWhile _ [] = []
myTakeWhile p (x:xs)
  | p x = x:(myTakeWhile p xs)
  | otherwise = []

myTakeWhile2 p = foldr add []
  where add x l
          | p x = x:l
          | otherwise = []

myDropWhile _ [] = []
myDropWhile p (x:xs)
  | p x = myDropWhile p xs
  | otherwise = x:xs

myDropWhile2 p = foldr add []
  where add x l
          | p x = l
          | otherwise = x:l

mySplitAt n xs = (myTake n xs, myDrop n xs)

mySpan p xs = (myTakeWhile p xs, myDropWhile p xs)
myBreak p xs = (myTakeWhile (not . p) xs, myDropWhile (not . p) xs)

myFilter _ [] = []
myFilter p (x:xs)
  | p x = x:(filter p xs)
  | otherwise = filter p xs

myElem x xs = not $ myNull $ myFilter (== x) xs  

myIsPrefixOf [] _ = True
myIsPrefixOf _ [] = False
myIsPrefixOf (y:ys) (x:xs)
  | x == y = myIsPrefixOf ys xs
  | otherwise = False

myIsInfixOf [] _ = True
myIsInfixOf _ [] = False
myIsInfixOf (y:ys) (x:xs)
  | myIsPrefixOf (y:ys) (x:xs) = True
  | otherwise = myIsInfixOf (y:ys) xs

t1 = myIsInfixOf [1, 2] [0, 1, 3, 2] == False || error ""

-- steps: 2*(length ys) + (length xs)
myIsSuffixOf ys xs = myIsPrefixOf (myReverse ys) (myReverse xs)

myDropLengthMaybe [] xs = Just xs
myDropLengthMaybe _  [] = Nothing
myDropLengthMaybe (_:xs) (_:ys) = myDropLengthMaybe xs ys

myDropLength [] xs = xs
myDropLength _  [] = []
myDropLength (_:xs) (_:ys) = myDropLength xs ys

-- steps: length xs + (length ys - length xs) + length xs
-- steps: length ys + length xs
myIsSuffixOf1 xs ys =
  -- (length xs) steps
  myIsPrefixOf xs tail
  where
    -- length delta  == length ys - length xs
    -- (length xs) steps
    delta = myDropLength xs ys
    -- length tail == length ys - length delta == length xs
    -- (length delta) steps
    tail = myDropLength delta ys

t2 = and [
  myIsSuffixOf1 [1,2,3,4] [1,2,3] == False || error "",
  myIsSuffixOf1 [1,2,3] [1,2,3] || error "",
  myIsSuffixOf1 [2,3] [1,2,3] || error ""]

myZipWith f _  [] = []
myZipWith f [] _  = []
myZipWith f (x:xs) (y:ys) = (f x y):myZipWith f xs ys

myZip xs ys = myZipWith (,) xs ys

myLines [] = []
myLines xs =
  case myBreak (== '\n') xs of
    (word, br:tail) -> word:myLines tail
    (word, _) -> [word]

myUnlines :: [String] -> String
myUnlines [] = ""
myUnlines (l:ls) = (l `myAppend`  "\n") `myAppend` (myUnlines ls)

myUnlines2 [] = ""
myUnlines2 ls = foldr step [] ls
  where step l r = l `myAppend` "\n" `myAppend` r

myWords [] = []
myWords str = word:myWords tailNoWhitespace
  where (word, tail) = myBreak whitespace str
        tailNoWhitespace = myDropWhile whitespace tail
        whitespace c = c `myElem` [' ', '\n', '\r', '\t']

myUnwords [] = []
myUnwords [str] = str
myUnwords (str:strs) =
  (str `myAppend` " ") `myAppend` (myUnwords strs)

mySafeHead [] = Nothing
mySafeHead (x:xs) = Just x
mySafeTail [] = Nothing
mySafeTail (x:xs) = Just xs
mySafeLast [] = Nothing
mySafeLast [x] = Just x
mySafeLast (_:xs) = mySafeLast xs
mySafeInit [] = Nothing
mySafeInit [x] = Just []
mySafeInit (x:xs) = do
  subInit <- mySafeInit xs
  return (x:subInit)

mySplitWith _ [] = []
mySplitWith p xs = split:mySplitWith p tailNotP
  where (split, tail) = myBreak p xs
        tailNotP = myDropWhile p tail

myReverseGroupBy p [] = []
myReverseGroupBy p xs = foldl' add [[]] xs
  where add ([]:gs) x  = [x]:gs
        add (g:gs) x
          | null $ myFilter (`p` x) g = [x]:g:gs
          | otherwise = (x:g):gs

-- Behaves like `Data.List(groupBy)`
-- Keeps adding to a group if new element is holds `p`
-- with any other element that's already in the group
-- while iterating from left to right
myGroupBy p xs = foldl' reverse [] $ myReverseGroupBy p xs
  where reverse r g = (myReverse2 g):r
-- λ> groupBy (<=) [1,2,3,1]
-- [[1,2,3,1]]
-- λ> groupBy (>) [1,3,2,1]
-- [[1],[3,2,1]]

-- Behaves like `Data.List.GroupBy(groupBy)`
-- Creates a new group as soon as two neighbouring elements
-- do not hold `p`
myGroupBy2 p [] = []
myGroupBy2 p xs = foldr add [[]] xs
  where add x ([]:gs) = [x]:gs
        add x ((y:ys):gs)
          | p x y = (x:y:ys):gs
          | otherwise = [x]:((y:ys):gs)
-- λ> myGroupBy2 (<=) [1,2,3,1]
-- [[1,2,3],[1]]
-- λ> myGroupBy2 (>) [1,3,2,1]
-- [[1],[3,2,1]]

myCycle xs = inner xs xs
  where inner [] xs = inner xs xs
        inner (y:ys) xs = y:inner ys xs

tests = t1 && t2
