module CFG.Data where

import Data.Maybe
import Data.List
import qualified Data.Map as Map

newtype Grammar = Grammar [Rule]
data Rule = Rule Nonterminal [Derivation]
newtype Derivation = Der [Symbol] deriving (Eq, Ord)

data Symbol = SNT Nonterminal
            | ST Terminal deriving (Eq, Ord)

type Nonterminal = String
type Terminal = String

mapRules :: (Rule -> a) -> Grammar -> [a]
mapRules f (Grammar rules) = map f rules

compressRules :: Grammar -> Grammar
compressRules (Grammar []) = error "empty grammar"
compressRules g@(Grammar rs) = 
    let startNt = startSymbol g
        ntMap = go rs Map.empty
        firstRuleDers = ntMap Map.! startNt
        ntMap' = Map.delete startNt ntMap 
    in  Grammar $ Rule startNt (sort firstRuleDers) : map (\(nt, der) -> Rule nt (sort der)) (Map.toList ntMap')
    where
        go [] ntMap = ntMap
        go ((Rule nt ders):rs) ntMap = go rs (Map.insertWith (++) nt ders ntMap)

startSymbol :: Grammar -> Nonterminal
startSymbol (Grammar []) = error "empty grammar"
startSymbol (Grammar (Rule nt _:_)) = nt

nonterminals :: Grammar -> [Nonterminal]
nonterminals (Grammar rs) = map (\(Rule nt _) -> nt) rs

terminals :: Grammar -> [Terminal]
terminals (Grammar rs) = concatMap fromRule rs
    where
        fromRule (Rule _ ders) = concatMap fromDer ders
        fromDer (Der syms) = mapMaybe fromSym syms
        fromSym (ST t) = Just t
        fromSym _      = Nothing

isTerminal :: Symbol -> Bool
isTerminal (ST _) = True
isTerminal _      = False

isNonterminal :: Symbol -> Bool
isNonterminal (SNT _) = True
isNonterminal _       = False

cfgSize :: Grammar -> Int
cfgSize (Grammar rules) = sum $ map ruleSize rules
    where
        ruleSize (Rule _ ders) = length ders

derEpsilon :: Terminal
derEpsilon = "_"

instance Show Grammar where
    show (Grammar rules) = intercalate "\n" (map show rules)

instance Show Rule where
    show (Rule nt ders) = nt ++ " -> " ++ intercalate " | " (map show ders)

instance Show Derivation where
    show (Der []) = derEpsilon
    show (Der syms) = concatMap show syms

instance Show Symbol where
    show (ST t) = case t of
        '\\':ts -> '\\':t
        _ | t == derEpsilon -> '\\':derEpsilon
        _ -> t
    show (SNT nt) = nt