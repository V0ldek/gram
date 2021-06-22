{-# LANGUAGE QuasiQuotes #-}
module CFG.Chomsky where

import CFG.QuasiQuoter
import CFG.Data
import Data.Function
import Data.Maybe
import Data.List
import Data.Bifunctor
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State

newtype ChomskyNF = ChomskyNF Grammar deriving Show
data ChomskySt = St {cfgNts :: Set.Set Nonterminal, cfgFresh :: Integer }
type ChomskyM = State ChomskySt

chomskyTest :: Grammar
chomskyTest = [cfg|
E -> T | EAT | AT
T -> F | TMF
F -> P | F^P
P -> num | var | (E)
A -> + | −
M -> * | /
|]
-- |
-- Chomsky normalisation
-- >>> chomskyNF chomskyTest
-- ChomskyNF A -> + | -
-- E -> AT | EN4 | FN6 | N1N7 | TN5 | num | var
-- F -> FN6 | N1N7 | num | var
-- M -> * | /
-- N0 -> AT | EN4 | FN6 | N1N7 | TN5 | num | var
-- N1 -> (
-- N2 -> )
-- N3 -> ^
-- N4 -> AT
-- N5 -> MF
-- N6 -> N3P
-- N7 -> EN2
-- P -> N1N7 | num | var
-- T -> FN6 | N1N7 | TN5 | num | var
chomskyNF :: Grammar -> ChomskyNF
chomskyNF g = evalState go (St Set.empty 0)
    where go = do
            unfolded <- cacheSymbols (unfoldRules g)
            intermediate <- applyStart unfolded >>= applyTerm >>= applyBin
            let normalised = applyDel intermediate & applyUnit
                compressed = compressRules normalised
            return $ ChomskyNF compressed

debugChomskyNF :: Grammar -> [Grammar]
debugChomskyNF g = evalState go (St Set.empty 0)
    where go = do
            unfolded <- cacheSymbols (unfoldRules g)
            intermediate <- applyStart unfolded >>= applyTerm >>= applyBin
            let normalised = applyDel intermediate & applyUnit
                compressed = compressRules normalised
            return [unfolded, intermediate, normalised, compressed]


cacheSymbols :: Grammar -> ChomskyM Grammar
cacheSymbols g =
    let nts = nonterminals g
    in  mapM_ addNonterminal nts >> return g

unfoldRules :: Grammar -> Grammar
unfoldRules g = Grammar $ concat $ mapRules go g
    where go (Rule nt ders) = map (\der -> Rule nt [der]) ders

applyStart :: Grammar -> ChomskyM Grammar
applyStart (Grammar []) = error "empty grammar"
applyStart (Grammar rules@((Rule start _) : rest)) = do
    nt <- freshNonterminal
    return $ Grammar $ Rule nt [Der [SNT start]] : rules

applyTerm :: Grammar -> ChomskyM Grammar
applyTerm g@(Grammar rules) = do
        let ts = nonsolitaryTerminals rules Set.empty
        renameList <- mapM genName (Set.toList ts)
        let rename = Map.fromList renameList
            rules' = map (modifyRule rename) rules
            newRules = map addRule (Map.toList rename)
        return $ Grammar (rules' ++ newRules)
    where
        nonsolitaryTerminals [] acc = acc
        nonsolitaryTerminals (Rule nt [Der [ST t]]:rs) acc = nonsolitaryTerminals rs acc
        nonsolitaryTerminals (Rule nt [Der der]:rs) acc = nonsolitaryTerminals rs (foldr (Set.insert . show) acc (filter isTerminal der))
        nonsolitaryTerminals _ _ = error "impossible"
        genName t = do
            nt <- freshNonterminal
            return (t, nt)
        modifyRule rename (Rule nt [Der [ST t]]) = Rule nt [Der [ST t]]
        modifyRule rename (Rule nt [Der syms]) = Rule nt [Der $ map (modifySym rename) syms]
        modifyRule _ _ = error "impossible"
        modifySym rename (ST t) = SNT $ rename Map.! t
        modifySym rename (SNT nt) = SNT nt
        addRule (t, nt) = Rule nt [Der [ST t]]

applyBin :: Grammar -> ChomskyM Grammar
applyBin (Grammar rules) = Grammar <$> go rules
    where
        go [] = return []
        go (r@(Rule nt [Der syms]):rest) =
            if length (filter isNonterminal syms) <= 2 then go rest >>= (\rs -> return (r:rs))
            else do
                nt' <- freshNonterminal
                let r' = Rule nt [Der [head syms, SNT nt']]
                rest' <- go (Rule nt' [Der (tail syms)]:rest)
                return (r':rest')
        go _ = error "impossible"

applyDel :: Grammar -> Grammar
applyDel (Grammar rules) =
    let nullables = findNullable rules Set.empty
        newRules = inlineNulls rules nullables []
        rules' = filter (\(Rule _ [Der syms]) -> not $ null syms) rules
    in  Grammar (rules' ++ newRules)
        where
            findNullable rules acc = let acc' = nullablePass rules acc in
                                    if Set.size acc == Set.size acc' then acc
                                    else findNullable rules acc'
            nullablePass (Rule nt [Der []]:rs) nullable = nullablePass rs (Set.insert (SNT nt) nullable)
            nullablePass (Rule nt [Der syms]:rs) nullable
                | all (`Set.member` nullable) syms = nullablePass rs (Set.insert (SNT nt) nullable)
            nullablePass (r:rs) acc = nullablePass rs acc
            nullablePass [] acc     = acc
            inlineNulls [] _ acc = acc
            inlineNulls (Rule nt [Der syms]:rs) nullable acc =
                inlineNulls rs nullable (map (\syms' -> Rule nt [Der syms']) $ variants syms nullable [])
            inlineNulls _ _ _ = error "impossible"
            variants [] _ acc = map reverse acc
            variants (ST t:syms) ns acc   = variants syms ns (map (\xs -> ST t:xs) acc)
            variants (SNT nt:syms) ns acc | SNT nt `Set.member` ns =
                variants syms ns (map (\xs -> SNT nt:xs) acc ++ acc)
            variants (SNT nt:syms) ns acc = variants syms ns (map (\xs -> SNT nt:xs) acc)

applyUnit :: Grammar -> Grammar
applyUnit (Grammar []) = error "empty grammar"
applyUnit g@(Grammar rules) =
    let startNt = startSymbol g 
        aliasesList = mapMaybe maybeAlias rules
        aliases = Map.fromListWith Set.union (map (second Set.singleton) aliasesList)
        newRules = generateAliasRules aliases rules []
        filtered = filter (isNothing . maybeAlias) newRules
        (startRules, rest) = partition (\(Rule nt _) -> nt == startNt) filtered
    in Grammar $ startRules ++ rest
    where
        maybeAlias (Rule nt [Der [SNT nt']]) = Just (nt', nt)
        maybeAlias _                         = Nothing
        generateAliasRules aliases [] acc = acc
        generateAliasRules aliases (r@(Rule nt der):rs) acc =
            let others = fromMaybe Set.empty (Map.lookup nt aliases)
                rules = map (`Rule` der) $ Set.toList others
            in case maybeAlias r of
                Just (nt', _) -> generateAliasRules aliases (rules ++ rs) acc
                Nothing       -> generateAliasRules aliases (rules ++ rs) (r:acc)

addNonterminal :: Nonterminal -> ChomskyM ()
addNonterminal nt = modify (\st -> st {cfgNts = Set.insert nt (cfgNts st)})

freshNonterminal :: ChomskyM Nonterminal
freshNonterminal = do
    n <- gets cfgFresh
    let name = 'N':show n
    exists <- gets (Set.member name . cfgNts)
    modify (\st -> st {cfgFresh = n + 1})
    if exists then freshNonterminal
              else addNonterminal name >> return name
