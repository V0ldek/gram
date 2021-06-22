module CFG.QuasiQuoter where

import CFG.Data
import qualified CFG.Parser as Parser
import Language.Haskell.TH
import Language.Haskell.TH.Quote

cfg :: QuasiQuoter
cfg = QuasiQuoter {
    quoteExp = quoteCfgExp
}

quoteCfgExp :: String -> Q Exp
quoteCfgExp s = do
    loc <- location
    let pos = (loc_filename loc, 
               fst $ loc_start loc,
               snd $ loc_start loc)
    cfg <- Parser.fullParseCfg pos s
    cfgToExpQ cfg

cfgToExpQ :: Grammar -> Q Exp
cfgToExpQ (Grammar rules) = do
    ruleEs <- mapM ruleToExpQ rules
    return $ ConE (mkName "Grammar") $$ ListE ruleEs

ruleToExpQ :: Rule -> Q Exp
ruleToExpQ (Rule nt ders) = do
    derEs <- mapM derToExpQ ders
    ntE <- stringE nt
    return $ (ConE (mkName "Rule") $$ ntE) $$ ListE derEs 

derToExpQ :: Derivation -> Q Exp
derToExpQ (Der syms) = do
    symEs <- mapM symToExpQ syms
    return $ ConE (mkName "Der") $$ ListE symEs

symToExpQ :: Symbol -> Q Exp
symToExpQ (SNT nt) = do
    ntE <- stringE nt
    return $ ConE (mkName "SNT") $$ ntE
symToExpQ (ST t) = do
    tE <- stringE t
    return $ ConE (mkName "ST") $$ tE

infixr 0 $$

($$) :: Exp -> Exp -> Exp 
a $$ b = AppE a b