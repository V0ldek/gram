{-# LANGUAGE QuasiQuotes #-}
module CFG.CYK where

import Control.Parallel
import Control.Parallel.Strategies
import Control.DeepSeq
import CFG.Chomsky
import CFG.Data
import CFG.QuasiQuoter
import Data.Array
import Data.List
import Data.Maybe
import qualified Data.Set as OldSet
import qualified Data.HashSet as Set

type CykEntry = (Int, Int, Nonterminal)
type CykSet = Set.HashSet CykEntry

cykExample :: Grammar
cykExample = [cfg|
S -> AB
A -> CD | CF
B -> c | EB
C -> a
D -> b
E -> c
F -> AD
|]

-- |
-- CYK
-- >>> cyk cykExample "aaabbbcc"
-- True
-- >>> cyk cykExample "aaabbcc"
-- False
cyk :: Grammar -> String -> Bool
cyk (Grammar []) _ = error "empty grammar"
cyk g s = let cnf = chomskyNF g
           in cykChomsky cnf s

cykEval :: Grammar -> String -> Bool
cykEval g s = let cnf = chomskyNF g
              in  cykChomsky cnf s

cykChomsky :: ChomskyNF -> String -> Bool
cykChomsky cnf@(ChomskyNF g) s =
    let initial = initCyk cnf s
        n = length s
        final = foldl' (cykLengthStep cnf n) initial [2..n]
    in
    Set.member (n, 1, startSymbol g) final

-- |
-- Init CYK
-- >>> let actual = initCyk (chomskyNF cykExample) "aaabbbcc"
-- >>> let expected = (Set.fromList [(1,1,"C"),(1,2,"C"),(1,3,"C"),(1,4,"D"),(1,5,"D"),(1,6,"D"),(1,7,"B"),(1,7,"E"),(1,8,"B"),(1,8,"E")])::CykSet
-- >>> actual == expected
-- True
initCyk :: ChomskyNF -> String -> CykSet
initCyk (ChomskyNF (Grammar rules)) s =
    let entries = findTerminals rules s []
    in  Set.fromList entries
    where
        findTerminals [] _ acc = acc
        findTerminals (Rule nt ders:rs) s acc =
            let matched = findDerivations ders s s 1 []
                entries = map (\(n, i) -> (n, i, nt)) matched
            in  findTerminals rs s (entries ++ acc)
        findDerivations [] _ _ _ acc = acc
        findDerivations (_:ds) [] s _ acc = findDerivations ds s s 1 acc
        findDerivations ds@(Der [ST t]:_) s' s n acc
            | t `isPrefixOf` s' = findDerivations ds (tail s') s (n+1) ((length t, n):acc)
        findDerivations ds s' s n acc = findDerivations ds (tail s') s (n+1) acc

cykLengthStep :: ChomskyNF -> Int -> CykSet -> Int -> CykSet
cykLengthStep _ n entries l
    | n < l = entries
cykLengthStep g n entries l =
    let starts = [1..n - l + 1]
        parts = parMap rseq (cykSpanStartStep g entries l) starts
    in Set.fromList (concat parts ++ Set.toList entries)

cykSpanStartStep :: ChomskyNF -> CykSet -> Int -> Int -> [CykEntry]
cykSpanStartStep g entries l s =
    let partitions = [1..l-1]
        parts = (if l <= 64 then map
                else parMap rseq) (cykPartitionStep g entries l s) partitions
    in concat parts

cykPartitionStep :: ChomskyNF -> CykSet -> Int -> Int -> Int -> [CykEntry]
cykPartitionStep (ChomskyNF (Grammar rules)) entries l s p =
    let newEntries = map (cykRuleStep l s p entries) rules
    in  catMaybes newEntries

cykRuleStep :: Int -> Int -> Int -> CykSet -> Rule -> Maybe CykEntry
cykRuleStep l s p entries (Rule nt ders) =
    if any (cykDerStep l s p entries) ders
    then Just (l, s, nt)
    else Nothing

cykDerStep :: Int -> Int -> Int -> CykSet -> Derivation -> Bool
cykDerStep l s p entries (Der [SNT nt1, SNT nt2])
    | Set.member (p, s, nt1) entries && Set.member (l-p, s+p, nt2) entries = True
cykDerStep _ _ _ _ _ = False

parUnion :: [CykSet] -> CykSet
parUnion []  = Set.empty
parUnion [x] = x
parUnion xs  =
  let (ys,zs) = splitAt (length xs `div` 2) xs
      ys'     = parUnion  ys
      zs'     = parUnion  zs
  in  zs' `par` (ys' `pseq` (ys' `Set.union` zs'))
