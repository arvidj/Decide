{-# LANGUAGE OverloadedStrings #-}

module Types where

-- import GHC.Generics
import Data.Aeson
import Data.Map.Strict hiding (map)
import Data.List
-- import Data.Aeson.Types.Internal.Parser

import Control.Applicative


type Point = (Double, Double)
type Puv = [ Bool ]
type Cmv = [ Bool ]
type Fuv = [ Bool ]
data BOp = ANDD | NOTUSED | ORR
           deriving (Show)


-- instance FromJSON BOp  where
--   parseJSON (Object v) =
--     fmap (\s -> case s of
--              "ANDD" -> ANDD
--              "ORR" -> ORR
--              "NOTUSED" -> NOTUSED) (decode v :: String)


instance FromJSON BOp  where
  parseJSON (String v) = case v of
    "ANDD" -> return ANDD
    "ORR" -> return ORR
    "NOTUSED" -> return NOTUSED
    _ -> fail "not a bop"

type Lcm = [[BOp]]
type Pum = [[Bool]]

showMat :: Show a => [[a]] -> String
showMat p = intercalate "\n" $ map show p

data Parameters = Parameters {
  radius1 :: Double,
  quads :: Integer,
  radius2 :: Double,
  dist :: Double,
  f_pts :: Integer,
  k_pts :: Integer,
  epsilon :: Double,
  length1 :: Double,
  g_pts :: Integer,
  q_pts :: Integer,
  n_pts :: Integer,
  c_pts :: Integer,
  area2 :: Double,
  length2 :: Double,
  b_pts :: Integer,
  d_pts :: Integer,
  area1 :: Double,
  a_pts :: Integer,
  e_pts :: Integer
} deriving (Show)

instance FromJSON Parameters where
  parseJSON (Object v) = Parameters <$>
    v .: "RADIUS1" <*>
    v .: "QUADS" <*>
    v .: "RADIUS2" <*>
    v .: "DIST" <*>
    v .: "F_PTS" <*>
    v .: "K_PTS" <*>
    v .: "EPSILON" <*>
    v .: "LENGTH1" <*>
    v .: "G_PTS" <*>
    v .: "Q_PTS" <*>
    v .: "N_PTS" <*>
    v .: "C_PTS" <*>
    v .: "AREA2" <*>
    v .: "LENGTH2" <*>
    v .: "B_PTS" <*>
    v .: "D_PTS" <*>
    v .: "AREA1" <*>
    v .: "A_PTS" <*>
    v .: "E_PTS"

-- data Person = Person {
--       name :: Text
--     , age  :: Int
--     } deriving (Generic, Show)


data Input = Input {
  random_seed :: Integer,
  parameters :: Parameters,
  numpoints :: Integer,
  points :: [ Point ],
  puv :: Puv,
  lcm :: Lcm
  } deriving (Show)

instance FromJSON Input where
  parseJSON (Object v) = Input <$>
    v .: "random_seed" <*>
    v .: "PARAMETERS" <*>
    v .: "NUMPOINTS" <*>
    (readPoints $ v .: "points") <*>
    v .: "PUV" <*>
    (readLcm $ v .: "LCM")

readPoints :: Functor a => a [[t]] -> a [(t,t)]
readPoints = fmap (map (\[a,b] -> (a,b)))

readLcm :: Functor a => a (Map String [BOp]) -> a Lcm -- [[Bop]]
readLcm v = fmap (\m -> map (\i -> m ! i) (map show [1])) v
                      -- [[ANDD]]

