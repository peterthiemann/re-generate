module Partitions where

-- | pn = partitions n
-- xs \in pn => sum xs = n, xi \in xs => xi > 0
-- no repetitions
partitions :: Int -> [[Int]]
partitions n
  | n == 0 = [[]]
  | otherwise = concatMap (\i -> map (i:) (partitions (n - i))) [1 .. n]


-- | pn = restrictedPartitions ns n
-- xs \in pn => sum xs = n, xi \in xs => xi \in ns /\ xi > 0
-- no repetitions
-- ns is sorted decreasingly
restrictedPartitions' :: [Int] -> Int -> [[Int]]
restrictedPartitions' [] n = [[]]
restrictedPartitions' ns n
  | n == 0 = [[]]
  | otherwise = let ns' = dropWhile (>n) ns in concatMap (\i -> map (i:) (restrictedPartitions' ns' (n - i))) ns'
