module Tests where

import Debug.Trace
import Test.QuickCheck
import System.Directory

import Types
import List
import Main
import Geom
import TestInput

import Prelude hiding (lcm)

-- checkQuad :: Point ->

bigCheck p = quickCheckWith (stdArgs { maxSuccess = 5000 }) p


type CFun = Double -> [Point] -> Bool

checkCircleThing :: CFun -> Point -> Point -> Point -> Point -> Double -> Property
checkCircleThing cfun p1 p2 p3 c rad =
  areInCircle p1 p2 p3 c rad ==> (cfun rad [p1, p2, p3])

checkCircleThing' :: CFun -> Point -> Point -> Point -> Point -> Double -> Property
checkCircleThing' cfun p1 p2 p3 c rad =
  not (cfun rad [p1, p2, p3]) ==>
  not (areInCircle p1 p2 p3 c rad)

checkCircleThing'' :: CFun -> Point -> Point -> Point -> Property
checkCircleThing'' cfun p1' p2' p3' =
  (d1 <= 1.0) && (d2 <= 1.0) && (d3 <= 1.0) ==>
  areInCircle p1 p2 p3 (0,0) 1.0 && (cfun 1.0 [p1, p2, p3])
  where trDbl p = p - (fromInteger $ truncate p)
        trPt (a,b) = (trDbl a, trDbl b)
        (p1,p2,p3) = (trPt p1', trPt p2', trPt p3')
        (d1,d2,d3) = let f = (distance (0,0)) in
                     (f p1,f p2,f p3)

checkCircleThingComp :: CFun -> CFun -> Point -> Point -> Point -> Double -> Bool
checkCircleThingComp cf cf' p1 p2 p3 r =
  (cf r [p1,p2,p3]) == (cf' r [p1,p2,p3])

-- generate a center, 3 pts, get distances from center to
-- points. check that inCircle with max of distance is true. it is
-- false if you reduce max.
-- checkCircleThing :: Point -> Point -> Point -> Point -> Double -> Property
-- checkCircleThing p1 p2 p3 c rad =
--   areInCircle p1 p2 p3 c rad ==> (inSomeCircle rad [p1, p2, p3])


angleSum p1 p2 p3 = angle p2 p1 p3 + angle p1 p2 p3 + angle p1 p3 p2

checkAngleSum :: Point -> Point -> Point -> Property
checkAngleSum p1 p2 p3 =
  isTriangle [p1, p2, p3] ==>
  abs (angleSum p1 p2 p3 - pi) < 0.01

type IPoint = (Integer, Integer)

iptop (a,b) = (fromInteger a, fromInteger b)

checkAngleSum' :: IPoint -> IPoint -> IPoint -> Property
checkAngleSum' p1' p2' p3' =
  let p1 = iptop p1' in
  let p2 = iptop p2' in
  let p3 = iptop p3' in
  -- p1 /= p2 && p2 /= p3 && p1 /= p3 ==>
  isTriangle [p1, p2, p3] ==>
  abs (angleSum p1 p2 p3 - pi) < 0.0001

-- TODO: check that consecutive n always recieves n points.

checkNub :: Eq a => [a] -> Bool
checkNub l = all (\c -> uniq c (nub l)) (nub l)
  where uniq c l = sum ([if c' == c then 1 else 0 | c' <- l]) == 1

checkPtPart :: Eq a => [a] -> Property
checkPtPart ls = length ls >= 2 ==>
  let (a, rest, b) = pt_part ls in
  a == head ls &&
  b == last ls &&
  rest == (take (length ls - 2) $ drop 1 ls)

checkConsecutive :: [Point] -> [Point] -> [Point] -> Property
checkConsecutive pts pre suf =
  length pts > 0 ==>
  consecutive (length pts) (pre ++ pts ++ suf) (== pts)

-- checkConsecutive' :: [Int] -> [Int] -> [Int] -> Property
-- checkConsecutive' pts pre suf =
--   length pts > 0 ==>
--   (consecutive (length pts) (pre ++ pts ++ suf) (/= pts) ==>
--   consecutive (length pts) (pre ++ suf) (/= pts))

checkSegment :: [[Int]] -> [Int] -> Property
checkSegment segs ls =
  length segs >= length ls - 1 ==>
  (segment segs'' ls'') == ls
  where segs' = take (length ls - 1) segs
        segs'' = (map length segs')
        ls'' = (segmentize segs' ls)

segmentize [] ls = ls
segmentize (s:ss) (l:ls) = (l:s ++ (segmentize ss ls))

checkConsSep :: [Int] -> [Int] -> [[Int]] -> [Int] -> Property
checkConsSep pre suf segs pts =
  length pts > 0 && length segs >= length pts - 1 ==>
  consSep (map length segs') (pre ++ pts' ++ suf) (== pts)
  where segs' = take (length pts - 1) segs
        pts' = (segmentize segs' pts)


-- | Checking lics

-- lic6


-- Input, expected result
type LicTest = (Input, Bool)
licTests :: [[LicTest]]
licTests =
  [
    -- 0
    [(test_input { points = [(-2,-2), (1,2), (1,1)],
                   parameters = (parameters test_input) { length1 = 1 }}, True),
     (test_input { points = [(0,0), (0,4)],
                   parameters = (parameters test_input) { length1 = 1 }}, False),
     (test_input { points = [(-2,-2), (1,2), (0,0), (1,1)],
                   parameters = (parameters test_input) { length1 = 1 }}, False)],
    -- 1
    [(test_input { points = [(-1,0), (0,1), (1,0)],
                   parameters = (parameters test_input) { radius1 = 1 }}, False),
     (test_input { points = [(-1,0), (0,1), (1,0)],
                   parameters = (parameters test_input) { radius1 = 0.5 }}, True),
     (test_input { points = [(-1,0), (-1,-1), (0,1), (1,0)],
                   parameters = (parameters test_input) { radius1 = 0.5 }}, True)],
    -- 2
    [(test_input { points = [(-1,-1), (0,0), (1,1)],
                   -- angle == 90 == pi
                   parameters = (parameters test_input) { epsilon = 0.1 }}, False),
     (test_input { points = [(-0.001,1), (0,0), (0.001,1)],
                   -- angle == close to 0
                   parameters = (parameters test_input) { epsilon = 0.1 }}, True),
     (test_input { points = [(-0.001,1), (0,0), (0,0), (0.001,1)],
                   -- angle == close to 0
                   parameters = (parameters test_input) { epsilon = 0.1 }}, False)
     ],
    -- 3
    [(test_input { points = [(0,0), (1,0), (1,1)],
                   parameters = (parameters test_input) { area1 = 0.5 }}, False),
     (test_input { points = [(0,0), (1,0), (1,1)],
                   parameters = (parameters test_input) { area1 = 0.4 }}, True),
     (test_input { points = [(0,0), (1,0), (1,0), (1,1)],
                    parameters = (parameters test_input) { area1 = 0.4 }}, False)],
    -- 4
    [(test_input { points = [(0.5,0.5), (-0.5,0.5), (-0.5,-0.5), (0.5,-0.5)],
                    parameters = (parameters test_input) { quads = 3,
                                                           q_pts = 4}}, True),
    (test_input { points = [(0.5,0.5), (-0.5,0.5), (-0.5,-0.5), (-0.5,-0.5), (0.5,-0.5)],
                    parameters = (parameters test_input) { quads = 3,
                                                           q_pts = 4}}, False)],
    -- 5
    [(test_input { points = [(1.0,0.5), (0.5,0.5)]}, True),
     (test_input { points = [(0.5,0.5), (1.0,0.5)]}, False)
    ],
    -- 6
    [
      (test_input { points = [(-1,0),(0,1),(1,0)],
                    parameters = (parameters test_input) { n_pts = 3, dist = 0.99 }}, True),
      (test_input { points = [(-1,0),(0,1),(1,0)],
                    parameters = (parameters test_input) { n_pts = 3, dist = 1.01 }}, False),
      (test_input { points = [(10,10),(0,0),(0,1),(0,6),(0,0),(10,10)],
                    parameters = (parameters test_input) { n_pts = 4, dist = 5 }}, True),
      (test_input { numpoints = 1 }, False)
    ],
    [(test_input { points = [(-2,-2), (1,2), (1,2), (1,2), (1,2), (1,1)],
                   parameters = (parameters test_input) { length1 = 1,
                                                          k_pts = 3}}, True),
    (test_input { points = [(0,2), (1,2), (1,2), (1,2), (1,1)],
                   parameters = (parameters test_input) { length1 = 1,
                                                          k_pts = 3}}, False)]
  ]


checkLic licNo testNo =
  let (inp, expRes) = licTests !! licNo !! testNo in
  (lics !! licNo) inp == expRes


checkLics :: [[LicTest]] -> [[Bool]]
checkLics tests =
  map (\(tests,lic) -> map (\(inp, expRes) -> lic inp == expRes) tests)
  (zip tests lics)


-- Checking pum

-- newtype ArbLcm15 = ArbLcm15 Pum
-- newtype ArbPum15 = ArbPum15 Pum

-- instance Arbitrary ArbLcm15 where
--   arbitrary = return $ ArbLcm15 [[False]]

instance Arbitrary BOp  where
  arbitrary = let choices = [ANDD, ORR, NOTUSED] in
              do n <- choose (0,length choices - 1)
                 return $ choices !! n

checkPum :: Lcm -> Cmv -> Property
checkPum lcm cmv =
  let lp = length cmv in
  all (\row -> length row == lp) lcm &&
  lp == length lcm ==>
  let pum = calc_pum lcm cmv in
  and [ pum !! i !! j ==
        (bopToFun (lcm !! i !! j))
        (cmv !! i)
        (cmv !! j)
      | i <- [0..(lp-1)], j <- [0..(lp-1)]]


pumExample :: Pum
pumExample = pad 15
  [pad 15 [x,     False, True, False] True, -- 0
   pad 15 [False, x,     True, True ] True,  -- 1
   pad 15 [True,  True,  x,    True ] True,    -- 2
   pad 15 [False, True,  True, x    ] True]   -- 3
  (repeatN 15 True)
x = True

checkPumExample =
  do putStrLn "Calculated PUM: "
     putStrLn (showMat $ normalizePum $ calc_pum lcm cmv)
     putStrLn "Expected PUM: "
     putStrLn (showMat $ normalizePum pumExample)
     putStrLn "Equality: "
     putStrLn (show $ normalizePum (calc_pum lcm cmv) == normalizePum pumExample)
  where cmv :: Cmv
        cmv = pad 15 [False, True, True, True] False

        lcm :: Lcm
        lcm =
          pad 15
          [pad 15 [ANDD, ANDD, ORR, ANDD]  NOTUSED,
           pad 15 [ANDD, ANDD, ORR, ORR] NOTUSED,
           pad 15 [ORR, ORR, ANDD, ANDD] NOTUSED,
           pad 15 [ANDD, ORR, ANDD, ANDD] NOTUSED]
          (repeatN 15 NOTUSED)

checkFuvExample =
  calc_fuv (test_input { puv = puvEx }) pumExample == fuvEx

fuvEx :: Fuv
fuvEx = pad 15 [False, True, True, False] True
puvEx = pad 15 [True, False] True

checkLaunchExample = calc_launch fuvEx == False


-- TODO: Check JSON encoding

-- checkJSON :: IO Bool
-- checkJSON = do let paths = (getDirectoryContents dirPath)
--                jsons <- mapM readFile paths
--                map (\(path, res) ->
--                      putStrLn (path ++ if res then " passed " else " failed"))
--                  zip paths (map verify jsons)
--     where dirPath = "./decode-test-input/parameters/parameters.json"
--           verify json =
--           maybe False
--             (\p -> toJSON p == (decode json :: Maybe Value))
--             (decode json :: Maybe Parameters)
