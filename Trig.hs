module Trig where
-- trig
distance (x1,y1) (x2,y2) =
  let (w, h) = (x2 - x1, y2 - y1) in
  sqrt (w*w + h*h)

inSomeCircle rad pts =
  or [distance p1 p2 <= 2*rad | p1 <- pts, p2 <- pts]

areInCircle p1 p2 p3 c rad =
  all (< rad) (map (distance c) [p1,p2,p3])

-- getLineFun (x1,y1) (x2,y2) =
--   (kx, m)
--   where kx = (y2-y1)/(x2-x1)
--         m = y1 - kx*x1
--       -- kx * x1 + m = y1

ptsToSides [p1,p2,p3] = (distance p1 p2, distance p1 p3, distance p2 p3)

-- 1. sum of any two sides is greater than third side (or)
isTriangle pts =
  let (a,b,c) = ptsToSides pts in
  and [a + b > c,
       b + c > a,
       a + c > b]

-- triangleArea :: [Point] -> Float
triangleArea pts =
  let (a,b,c) = ptsToSides pts in
  let p = (a+b+c) / 2 in
  sqrt (p * (p-a) * (p - b) * (p - c))

-- using law of cosines
angle p1 v p2 =
  -- cos C = (a^2 + b^2 - c^2)/(2ab)
  acos ((a**2 + b**2 - c**2)/(2*a*b))
  where a = distance v p1
        b = distance v p2
        c = distance p1 p2

-- I: (0,0), (0, 1), (1, 0)
quadrant (x,y) | (x >= 0 && y >= 0) = 0 -- I
-- II: (-1,0)
quadrant (x,y) | (x < 0 && y >= 0) = 1 -- II
-- III: (0,-1),
quadrant (x,y) | (x < 0 && y < 0) = 2 -- III
-- IV: ?
quadrant (x,y) | (x >= 0 && y < 0) = 3 -- III

rad2ang r = r/pi* 180
