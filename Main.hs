{-# LANGUAGE OverloadedStrings #-}

-- import Test.JSON

import Prelude hiding (lcm)

import Data.Aeson
import System.Environment
import qualified Data.ByteString.Lazy as BSLazy

import Debug.Trace
import Test.QuickCheck

import Types
import TestInput
import Trig
import List

type Lic = Input -> Bool

-- More specific helpers

withNpoints :: Integer -> Input -> Bool -> Bool
withNpoints n inp f =
  if numpoints inp < n
  then False
  else f

checkAngle :: Double -> Double -> [Point] -> Bool
checkAngle ang eps [p1, vertex, p3] =
  if p1 == vertex || p3 == vertex then
    False
  else
    let a = angle p1 vertex p3 in
    (a < ang - eps || a > ang + eps)

-- triangleOfArea :: Double -> [Point] -> Bool
-- triangleOfArea area pts =
--   isTriangle pts && triangleArea pts > area

-- Lics

lic0,lic1,lic2,lic3,lic4,lic5,lic6,lic7,lic8, lic9,lic10,lic11,lic12,lic13,lic14 :: Lic
lic0 inp =
  let llength1 = length1 $ parameters inp in
  consecutive 2 (points inp) (\[p1, p2] -> distance p1 p2 <= llength1)
lic1 inp =
  let rad = (radius1 $ parameters inp) in
  consecutive 3 (points inp) (not . inSomeCircle rad)
lic2 inp =
  let eps = (epsilon $ parameters inp) in
  consecutive 3 (points inp) (checkAngle pi eps)

lic3 inp =
  let aarea1 = (area1 $ parameters inp) in
  consecutive 3 (points inp)
  (\pts -> isTriangle pts && triangleArea pts > aarea1)

lic4 inp =
  let nq_pts = fromInteger $ q_pts $ parameters inp in
  let nquads = fromInteger $ quads $ parameters inp in
  -- quad /distinct/ quadrants?
  consecutive nq_pts (points inp) (\pts -> nquads < (length $ nub $ map quadrant pts))

lic5 inp =
  consecutive 2 (points inp) (\[(xi,_),(xj,_)] -> xj - xi < 0)

lic6 inp =
  let nn_pts = fromInteger $ n_pts $ parameters inp in
  let ddist = dist $ parameters inp in
    withNpoints 3 inp $ (consecutive nn_pts (points inp) (check_dist nn_pts ddist))
  where check_dist :: Int -> Double -> [Point] -> Bool
        check_dist nn_pts ddist pts =
          let (pfirst, rest, plast) = pt_part pts in
          let distances =
                if pfirst == plast then
                  -- theory 2: coincident is either the first/last?
                  -- (this is what i'm going with)
                  map (distance pfirst) rest
                  -- theory 1: coincident is any of the points not
                  -- first/last?
                else
                  map (\p ->
                        let ab = distance pfirst plast in
                        let ac = distance pfirst p in
                        sqrt (ac**2 - (ab/2)**2)) rest
          in any (> ddist) distances

lic7 inp =
  let nk_pts = fromInteger $ k_pts $ parameters inp in
  let llength1 = length1 $ parameters inp in
  withNpoints 3 inp $
  consSep [nk_pts] (points inp)
  (\[f,l] -> distance f l > llength1)
  -- consecutive nk_pts (points inp)
  -- (\pts -> let (f,_,l) = pt_part pts in distance f l > llength1)

lic8 inp =
  let na_pts = fromInteger $ a_pts $ parameters inp in
  let nb_pts = fromInteger $ b_pts $ parameters inp in
  let rradius1 = (radius1 $ parameters inp) in
  withNpoints 5 inp $
  consSep [na_pts, nb_pts] (points inp) (not . inSomeCircle rradius1)

lic9 inp =
  let nc_pts = fromInteger $ c_pts $ parameters inp in
  let nd_pts = fromInteger $ d_pts $ parameters inp in
  let eps = (epsilon $ parameters inp) in
  withNpoints 5 inp $
  consSep [nc_pts, nd_pts] (points inp) (checkAngle pi eps)

lic10 inp =
  let ne_pts = fromInteger $ e_pts $ parameters inp in
  let nf_pts = fromInteger $ f_pts $ parameters inp in
  let aarea1 = (area1 $ parameters inp) in
  withNpoints 5 inp $
  consSep [ne_pts, nf_pts] (points inp) (\pts -> isTriangle pts && triangleArea pts > aarea1)

lic11 inp =
  let ng_pts = fromInteger $ g_pts $ parameters inp in
  withNpoints 3 inp $
  consSep [ng_pts] (points inp) (\[(xi,_),(xj,_)] -> xj - xi < 0)

lic12 inp =
  let nk_pts = fromInteger $ k_pts $ parameters inp in
  let llength1 = length1 $ parameters inp in
  let llength2 = length1 $ parameters inp in
  withNpoints 3 inp $
  consSep [nk_pts] (points inp) (\[f,l] -> distance f l > llength1)
  && consSep [nk_pts] (points inp) (\[f,l] -> distance f l < llength2)

lic13 inp =
  let na_pts = fromInteger $ a_pts $ parameters inp in
  let nb_pts = fromInteger $ b_pts $ parameters inp in
  let rradius1 = (radius1 $ parameters inp) in
  let rradius2 = (radius2 $ parameters inp) in
  withNpoints 5 inp $
  consSep [na_pts, nb_pts] (points inp) (not . inSomeCircle rradius1)
  && consSep [na_pts, nb_pts] (points inp) (inSomeCircle rradius2)

lic14 inp =
  let ne_pts = fromInteger $ e_pts $ parameters inp in
  let nf_pts = fromInteger $ f_pts $ parameters inp in
  let aarea1 = (area1 $ parameters inp) in
  let aarea2 = (area2 $ parameters inp) in
  withNpoints 5 inp $
  consSep [ne_pts, nf_pts] (points inp) (\pts -> isTriangle pts && triangleArea pts > aarea1) &&
  consSep [ne_pts, nf_pts] (points inp) (\pts -> isTriangle pts && triangleArea pts < aarea2)

lics :: [Lic]
lics = [
  lic0,lic1,lic2,lic3,lic4,lic5,lic6,lic7,lic8,
  lic9,lic10,lic11,lic12,lic13,lic14
  ]

-- Main functions

calc_cmv :: Input -> Cmv
calc_cmv inp = map (\lic -> lic inp) lics

calc_pum :: Lcm -> Cmv -> Pum
calc_pum lcm cmv =
  let lcmf = map (map bopToFun) lcm
      -- pum[i,j] = cmv[i] (Lcm[i,j]) cmv[j]
  in [[ cmv_i `bop_ij` cmv_j | (cmv_i, bop_ij) <- zip cmv bop_j ]
        | (cmv_j, bop_j) <- (zip cmv lcmf)]

bopToFun :: BOp -> (Bool -> Bool -> Bool)
bopToFun p = case p of
  ANDD -> (&&)
  ORR -> (||)
  NOTUSED -> (\_ _ -> True)

--calc_pum  [[ANDD, ORR],
--           [NOTUSED, ORR]]
--           [True, False]
-- [[ - , True ]
--  [True, -]

calc_fuv :: Input -> Pum -> Fuv
calc_fuv inp pum =
  let pum' = normalizePum pum in
  [ (not puv_i) || (and pum_i) | (puv_i, pum_i) <- zip (puv inp) pum ]

normalizePum :: Pum -> Pum
normalizePum pum = [take i p ++ [True] ++ drop (i + 1) p
                   | (i, p) <- zip [0..length pum] pum]


calc_launch :: Fuv -> Bool
calc_launch fuv = and fuv

decide :: Input -> Bool
decide inp =
  let cmv = calc_cmv inp in
  let pum = calc_pum (lcm inp) cmv in
  let fuv = calc_fuv inp pum in
  calc_launch fuv

main :: IO ()
main = do args <- getArgs
          s <- BSLazy.readFile (head args)
          let inp = decode s :: Maybe Input
          case inp of
            Just inp ->
              do -- putStrLn (show $ inp)
                 putStrLn (if decide inp then "YES" else "NO")
            Nothing ->
              putStrLn "Could not read input"
