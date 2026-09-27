module Util where

import qualified Data.Map.Strict as M

counter :: (Ord a) => [a] -> M.Map a Int
counter = foldr (M.alter (\v -> case v of {Nothing -> Just 1; Just n -> Just (n+1)})) M.empty

cartesianProdMap :: (Ord k) => M.Map k [v] -> [M.Map k v]
cartesianProdMap m =
    let keys = M.keys m
        valRanges = M.elems m
        valCombos = sequence valRanges
    in
    [M.fromList (zip keys valCombo) | valCombo <- valCombos]

partition :: (Ord k) => (v -> k) -> [v] -> M.Map k [v]
partition f xs =
    let add x m = if M.member (f x) m then M.adjust (x:) (f x) m else M.insert (f x) [x] m
    in
    foldr add M.empty xs

