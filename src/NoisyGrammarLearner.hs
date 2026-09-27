{-# OPTIONS_GHC -Wno-unused-imports #-}

module NoisyGrammarLearner where

import qualified Data.Map.Strict as M

import HypothesisSpace (HypothesisSpace, readHypothesisSpace)
import Tree (Tree, readTrees)
import Likelihood (getProbForTrees)

----------------------------------------------

-- e.g. liftA2 allLikelihoods (readHypothesisSpace "examples/word-order-mini-hypspace-Fig6b.yaml") (readTrees "examples/word-order-mini-data-Fig8a.trees")
allLikelihoods :: (Ord a, Ord b) => HypothesisSpace a b -> [Tree a b] -> M.Map String Double
allLikelihoods hs trees = M.map (\g -> getProbForTrees g trees) hs

posterior :: (Ord a, Ord b) => HypothesisSpace a b -> [Tree a b] -> M.Map String Double
posterior hs trees =
    let likelihoods = allLikelihoods hs trees in
    M.map (\x -> x / (sum (M.elems likelihoods))) likelihoods

