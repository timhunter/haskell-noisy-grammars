module Likelihood (getProbForTrees) where

import Numeric.SpecFunctions (choose, logGamma)
import Control.Exception (assert)
import qualified Data.Map.Strict as M

import Util (counter, cartesianProdMap, partition)
import Grammar (Grammar, RHS, Mode(..), lhs, rhs, getAlphas, getStartSymbol)
import Tree (Tree, root, rewrites)

getProbForTrees :: (Ord a, Ord b) => Grammar a b -> [Tree a b] -> Double
getProbForTrees g ts =
    if all (\t -> root t == getStartSymbol g) ts then
        let allRewrites = concatMap rewrites ts in
        let rewriteLists = M.map (map rhs) (partition lhs allRewrites) in
        let rewriteCounters = M.map counter rewriteLists in
        product [getProbForOneNonterm (getAlphas g nt Core) (getAlphas g nt Noise) c | (nt,c) <- M.toList rewriteCounters]
    else
        0

getProbForOneNonterm :: (Ord a, Ord b) => M.Map (RHS a b) Double -> M.Map (RHS a b) Double -> M.Map (RHS a b) Int -> Double
getProbForOneNonterm coreAlphas noiseAlphas rewriteCounts =

    -- Every key in the rewriteCounts map should also be a key in noiseAlphas
    assert (all (\x -> M.member x noiseAlphas) (M.keys rewriteCounts)) $

    -- Every key in the coreAlphas map should also be a key in noiseAlphas
    assert (all (\x -> M.member x noiseAlphas) (M.keys coreAlphas)) $

    let splitsForRHS r total =
            let maxFromCore = if M.member r coreAlphas then total else 0 in
            [(n, total-n) | n <- [0 .. maxFromCore]]
    in

    let splitsForEachRHS = M.mapWithKey splitsForRHS rewriteCounts
        allSplitMaps = cartesianProdMap splitsForEachRHS
    in

    sum [getProbForNontermWithSplits coreAlphas noiseAlphas splitMap | splitMap <- allSplitMaps]

-- rewriteSplitCounts maps a RHS to a pair of frequencies (core,noise)
getProbForNontermWithSplits :: (Ord a, Ord b) => M.Map (RHS a b) Double -> M.Map (RHS a b) Double -> M.Map (RHS a b) (Int,Int) -> Double
getProbForNontermWithSplits coreAlphas noiseAlphas rewriteSplitCounts =
    let
        -- there's one multinomial term for each rhs (total-number-of-that-rhs choose noise-number-of-that-rhs)
        multinomialTerm = product [choose (c+n) c | (c,n) <- M.elems rewriteSplitCounts]
        -- there's one integral term for the core rhss (parametrized by a vector of core counts; with zeros, in effect, for any necessarily-noise rhss)
        coreTerm = integral coreAlphas (M.map fst rewriteSplitCounts)
        -- there's one integral term for the noise rhss (parametrized by a vector of noise counts)
        noiseTerm = integral noiseAlphas (M.map snd rewriteSplitCounts)
        -- there's one integral term for the core-noise split (parametrized by a pair: total-core, total-noise)
        (coreTotal, noiseTotal) = M.foldr (\(x1,y1) (x2,y2) -> (x1+x2,y1+y2)) (0,0) rewriteSplitCounts
        splitTerm = integral (M.fromList [(Core,1),(Noise,1)]) (M.fromList [(Core,coreTotal),(Noise,noiseTotal)])
    in
    multinomialTerm * coreTerm * noiseTerm * splitTerm

-- assumes that every key in the counts map is also a key in the alphas map 
-- (i.e. everything that was observed has an associated alpha value)
integral :: (Ord k) => M.Map k Double -> M.Map k Int -> Double
integral alphas counts =
    let gamma x = exp (logGamma x)
        beta m = product (map gamma (M.elems m)) / gamma (sum (M.elems m))
        extendedCounts = M.union counts (M.fromList [(k,0) | k <- M.keys alphas, not (M.member k counts)])
    in
    let num = beta (M.intersectionWith (\a n -> a + fromIntegral n) alphas extendedCounts)
        denom = beta alphas
    in
    num / denom

