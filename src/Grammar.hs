module Grammar (Grammar, Rule, RHS, Mode(..), brule, lrule, lhs, rhs, makeGrammar, getAlphas, getStartSymbol) where

import qualified Data.Map.Strict as M

data Grammar a b = Grammar a [Rule a b] [Rule a b] deriving Show

data RHS a b = BRHS [a] | LRHS b deriving (Show,Eq,Ord)

data Rule a b = Rule (a, RHS a b) deriving (Show,Eq)

data Mode = Core | Noise deriving (Show,Eq,Ord)

brule :: a -> [a] -> Rule a b
brule x ys = Rule (x, BRHS ys)

lrule :: a -> b -> Rule a b
lrule x y = Rule (x, LRHS y)

lhs :: Rule a b -> a
lhs (Rule (x,_)) = x

rhs :: Rule a b -> RHS a b
rhs (Rule (_,x)) = x

-- TODO: remove duplicate rules?
makeGrammar :: a -> [Rule a b] -> [Rule a b] -> Grammar a b
makeGrammar = Grammar

getRules :: Grammar a b -> Mode -> [Rule a b]
getRules (Grammar _ core _)  Core  = core
getRules (Grammar _ _ noise) Noise = noise

getAlphas :: (Ord a, Ord b) => Grammar a b -> a -> Mode -> M.Map (RHS a b) Double
getAlphas g nt mode =
    M.fromList [(r,1) | Rule (l,r) <- getRules g mode, l == nt]

getStartSymbol :: Grammar a b -> a
getStartSymbol (Grammar x _ _) = x

