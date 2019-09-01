import Data.List

data Point a = Point a a deriving (Eq, Show)
data TurnDirection = LEFT | STRAIGHT | RIGHT deriving (Eq, Show)

cosOf (Point x y) =
  x / sqrt (x^2 + y^2)

minus (Point x y) (Point x0 y0) = Point (x - x0) (y - y0)

isUpperHalf (Point _ y) = y >= 0
angle p
  | isUpperHalf p =        acos (cosOf p)
  | otherwise     = 2*pi - acos (cosOf p)

epsilon = 0.000000001
turnDirection  a b c   = turnDirection0 a b c epsilon
turnDirection0 a b c e
  | abs alpha < e  = STRAIGHT
  | alpha     < pi = LEFT
  | alpha     > pi = RIGHT
  where
    alpha = normalize $ angle vc - angle vb
    vc = minus c b
    vb = minus b a
    normalize alpha
      | alpha >= 0 = alpha
      | otherwise  = 2*pi + alpha

turnDirection1 a b c = turnDirection0 a b c epsilon
turnDirection10 a b c e
  | isUpperHalf vc && not (isUpperHalf vb) = LEFT
  | not (isUpperHalf vc) && isUpperHalf vb = RIGHT
  | abs (cosOfC - cosOfB) <= e             = STRAIGHT
  | isUpperHalf vc && cosOfC - cosOfB < e  = LEFT
  | isUpperHalf vc && cosOfB - cosOfC < e  = RIGHT
  | cosOfC - cosOfB < e                    = RIGHT
  | cosOfB - cosOfC < e                    = LEFT
  where
    vc = minus c b
    vb = minus b a
    cosOfC = cosOf vc
    cosOfB = cosOf vb

turnDirection2 a b c = turnDirection20 a b c epsilon
turnDirection20 a b c e
  | crossProductBC > e     = LEFT
  | abs crossProductBC < e = STRAIGHT
  | crossProductBC < e     = RIGHT
  where
    crossProductBC = crossProduct vb vc
    vc = c `minus` b
    vb = b `minus` a
    crossProduct (Point a1 b1) (Point a2 b2) =
      a1*b2 - a2*b1

foldr3 :: (a -> a -> a -> r -> r) -> r -> [a] -> r
foldr3 _ z []         = z
foldr3 _ z [a]        = z
foldr3 _ z [a, b]     = z
foldr3 f z (a:b:c:ps) = f a b c (foldr3 f z (b:c:ps))

turnDirections ps = foldr3 step [] ps
  where step a b c r = (turnDirection a b c):r

lowest ps = minimumBy lowest ps
  where lowest (Point x1 y1) (Point x2 y2)
          | y1 < y2              = LT
          | y1 == y2 && x1 < x2  = LT
          | y1 == y2 && x1 == x2 = EQ
          | y1 == y2 && x1 > x2  = GT
          | otherwise            = GT

sortByAngle ps = sortBy angleToLowest ps
  where angleToLowest p1 p2
          | p1 == p0       = LT
          | p2 == p0       = GT
          | cosV1 <  cosV2 = GT
          | cosV1 == cosV2 = EQ
          | otherwise      = LT
          where
            cosV1 = cosOf v1
            cosV2 = cosOf v2
            v1 = p1 `minus` p0
            v2 = p2 `minus` p0
            p0 = lowest ps

grahamScan [] = []
grahamScan [p1] = [p1]
grahamScan [p1, p2] = [p1, p2]
grahamScan ps = foldl step [t2, t1] ts
  where t1:t2:ts = sortByAngle ps
        step (r2:r1:rs) p
          | turnDirection r1 r2 p == LEFT = p:r2:r1:rs
          | otherwise                     = step (r1:rs) p


test (a, b, c) r =
  turnDirection a b c == r ||
  error ("" ++ show (a, b, c) ++ " != " ++ show r)
t1 = (Point 0.0 0.0, Point 1.0 0.0, Point 2.0 1.0)
r1 = test t1 LEFT
t2 = (Point 0.0 0.0, Point 1.0 0.0, Point 2.0 (-1.0))
r2 = test t2 RIGHT
t3 = (Point 0.0 0.0, Point 1.0 1.0, Point 2.0 1.0)
r3 = test t3 RIGHT
t4 = (Point 0.0 0.0, Point 1.0 1.0, Point 1.0 2.0)
r4 = test t4 LEFT
t5 = (Point 1.0 (-1.0), Point 2.0 (-2.0), Point 2.0 (-3.0))
r5 = test t5 RIGHT
t6 = (Point 1.0 (-1.0), Point 2.0 (-2.0), Point 2.0 0.0)
r6 = test t6 LEFT
t7 = (Point (-1.0) 1.0, Point 0.0 1.0, Point (-1.0) 0.0)
r7 = test t7 RIGHT
t8 = (Point 1.0 1.0, Point 2.0 2.0, Point 3.0 3.0)
r8 = test t8 STRAIGHT
t9 = (Point 1.1 1.1, Point 2.2 2.2, Point 3.3 3.3)
r9 = test t9 STRAIGHT
r10 =
  foldr3 (\a b c r -> (a, b, c):r) [] [1, 2, 3, 4, 5] ==
  [(1,2,3),(2,3,4),(3,4,5)] || error ""
test1 (a, b, c) r =
  turnDirection1 a b c == r ||
  error ("" ++ show (a, b, c) ++ " != " ++ show r)
r11 = test1 t1 LEFT
r12 = test1 t2 RIGHT
r13 = test1 t3 RIGHT
r14 = test1 t4 LEFT
r15 = test1 t5 RIGHT
r16 = test1 t6 LEFT
r17 = test1 t7 RIGHT
r18 = test1 t8 STRAIGHT
r19 = test1 t9 STRAIGHT
r20 = lowest [Point 0.0 0.0, Point 1.0 0.0, Point 2.0 1.0] ==
  Point 0.0 0.0
-- x | x
-- --xx-
--   |
ps1 = [Point 0.0 0.0, Point 1.0 0.0, Point 2.0 1.0, Point (-2) 1]
-- x | x
--   x
-- --xx-
--   |
ps2 = [Point 0.0 0.0, Point 1 0, Point 0 1, Point (-2) 2, Point 2 2]
-- x | x
--   x
-- --x--
--   |
ps3 = [Point 0.0 0.0, Point 0 1, Point (-2) 2, Point 2 2]
-- x | x
--   |
-- --x--
--   |
--   x
ps4 = [Point 0.0 0.0, Point 0 (-2), Point (-2) 2, Point 2 2]
-- x | x
--   |x
-- --x--
--   |
--   |x
ps5 = [Point 0.0 0.0, Point 1 (-2), Point 1 1, Point (-2) 2, Point 2 2]
test2 (a, b, c) r =
  turnDirection2 a b c == r ||
  error ("" ++ show (a, b, c) ++ " != " ++ show r)
r21 = test2 t1 LEFT
r22 = test2 t2 RIGHT
r23 = test2 t3 RIGHT
r24 = test2 t4 LEFT
r25 = test2 t5 RIGHT
r26 = test2 t6 LEFT
r27 = test2 t7 RIGHT
r28 = test2 t8 STRAIGHT
r29 = test2 t9 STRAIGHT
r30 = turnDirections ps5 == [LEFT,LEFT,RIGHT] || error ""
testResults =
  r1 && r2 && r3 && r4 && r5 && r6 && r7 && r8 && r9 && r10 &&
  r11 && r12 && r13 && r14 && r15 && r16 && r17 && r18 && r19 &&
  r20 &&
  r21 && r22 && r23 && r24 && r25 && r26 && r27 && r28 && r29 &&
  r30
