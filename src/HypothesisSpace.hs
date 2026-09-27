{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module HypothesisSpace (HypothesisSpace, readHypothesisSpace) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.List as L

import Data.Yaml (decodeFileEither, (.:), (.:?), (.!=), prettyPrintParseException, FromJSON(..), Value(..))
import Data.Aeson.Types (prependFailure, typeMismatch)

import Grammar (Grammar, Rule, makeGrammar, brule, lrule)

type HypothesisSpace a b = M.Map String (Grammar a b)

readHypothesisSpace :: FilePath -> IO (HypothesisSpace String String)
readHypothesisSpace file =
    do
        e <- decodeFileEither file
        case e of
            Left exception -> fail (prettyPrintParseException exception)
            Right yamlObj -> return (hypothesisSpaceFromConfig yamlObj)

hypothesisSpaceFromConfig :: HypothesisSpaceConfig -> HypothesisSpace String String
hypothesisSpaceFromConfig (HypothesisSpaceConfig allCoreRulesAsSharedNoise startSymbol sharedPairOfRuleLists grammarsMap) =
    let PairOfRuleLists sharedCore sharedNoise = sharedPairOfRuleLists
        allCore = L.nub (sharedCore ++ concat [core | PairOfRuleLists core _ <- M.elems grammarsMap])
        additionalSharedNoise = if allCoreRulesAsSharedNoise then allCore else []
        helper (PairOfRuleLists core noise) = makeGrammar startSymbol
                                                          (core ++ sharedCore)
                                                          (noise ++ sharedNoise ++ additionalSharedNoise)
    in
    M.map helper grammarsMap

------------------------------------------------------------------------------------------

data HypothesisSpaceConfig = HypothesisSpaceConfig Bool String PairOfRuleLists (M.Map String PairOfRuleLists) deriving Show
instance FromJSON HypothesisSpaceConfig where
    parseJSON (Object o) =
        HypothesisSpaceConfig <$> (o .:? "AllCoreRulesAsSharedNoise" .!= False)
                              <*> (o .:? "StartSymbol" .!= "S")
                              <*> (o .:? "Shared" .!= (PairOfRuleLists [] []))
                              <*> (o .: "Grammars")
    parseJSON other = prependFailure "parsing HypothesisSpaceConfig failed, " (typeMismatch "Object" other)

data PairOfRuleLists = PairOfRuleLists [Rule String String] [Rule String String] deriving Show
instance FromJSON PairOfRuleLists where
    parseJSON (Object o) =
        PairOfRuleLists <$> (o .:? "core" .!= [])
                        <*> (o .:? "noise" .!= [])
    parseJSON other = prependFailure "parsing PairOfRuleLists failed, " (typeMismatch "Object" other)

-- (1) Can we make this more general, for something more like Rule a b? If so, we could remove FlexibleInstances.
-- (2) This gives an `orphan instance' warning. I'm tentatively ignoring it for now. Could be fixed with a newtype wrapper.
instance FromJSON (Rule String String) where
    parseJSON (String t) =
        let s = T.unpack t
            toks = words s
        in
        if (length toks > 2) && (toks!!1 == "->") then
            return (brule (toks!!0) (drop 2 toks))
        else if (length toks == 2) then
            return (lrule (toks!!0) (toks!!1))
        else
            fail ("Ill-formed rule: " ++ s)
    parseJSON other = prependFailure "parsing Rule failed, " (typeMismatch "String" other)

