module List where

-- List functions
nub :: Eq a => [a] -> [a]
nub [] = []
nub (l:ls) = l:(nub (filter (/=l) ls))

repeatN :: Int -> a -> [a]
repeatN n x = take n $ repeat x

pad :: Int -> [a] -> a -> [a]
pad  n ls d = ls ++ (repeatN (n - length ls) d)

pt_part :: [a] -> (a,[a],a)
pt_part ls =
  if length ls < 2 then
    error "list to short"
  else
    let (pfirst, plast) = (head ls, last ls) in
    let rest = take (length ls - 2) $ tail ls in
    (pfirst, rest, plast)

-- points handling
consecutive :: Int -> [a] -> ([a] -> Bool) -> Bool
consecutive n ls f | length ls >= n =
  f (take n ls) || consecutive n (tail ls) f
consecutive n ls f = False

-- segment [] [1,_,_,3] == [1,_,_,3]
-- segment [2] [1,_,_,3] == [1,3]
-- segment [2,3] [1,_,_,3,_,_,_,4] == [1,3,4]
segment :: [Int] -> [a] -> [a]
segment [] pts = pts
segment ls pts = if sum ls + length ls + 1 /= length pts
                 then error "incorrect partition spec"
                 else segment' ls pts
  where segment' :: [Int] -> [a] -> [a]
        segment' [] [p] = [p]
        segment' (l:ls) (p:pts) = p:(segment ls (drop l pts))

consSep :: [Int] -> [a] -> ([a] -> Bool) -> Bool
consSep seps pts f =
  let segLen = sum seps + length seps + 1 in
  consecutive segLen pts (\pts -> f (segment seps pts))

