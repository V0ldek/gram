{-# LANGUAGE QuasiQuotes #-}
module Main where

import CFG.CYK
import CFG.Chomsky
import CFG.QuasiQuoter (cfg)
import CFG.Data
import System.Environment

parenCfg :: Grammar
parenCfg = [cfg| A -> _ | AA | (A) |]

-- |
-- example CFG
-- >>> exampleCfg
-- A -> aA | B
-- B -> b | _
exampleCfg :: Grammar
exampleCfg = [cfg|
A -> aA | B
B -> b | _
|]

cykTest :: Int -> Grammar
cykTest n = Grammar (map (generateRule n) [0..n])
    where
        generateRule n i | n == i = Rule (name i) [Der [], Der [ST "a"]]
        generateRule n i = Rule (name i) [
            Der [],
            Der [ST "a", SNT (name (i+1))],
            Der [SNT (name (i+1)), ST "a"],
            Der [ST "a"],
            Der [SNT (name (i+1))]]
        name i = 'A':show i

cykTest5 :: Grammar
cykTest5 = [cfg|
A0 -> a | aA1 | A1a | _
A1 -> a | aA2 | A2a | _
A2 -> a | aA3 | A3a | _
A3 -> a | aA4 | A4a | _
A4 -> a | aA5 | A5a | _
A5 -> a | _
|]

-- |
-- cykTest size
-- >>> let ChomskyNF g = chomskyNF $ cykTest 100
-- >>> cfgSize g
-- 15553
--
-- >>> let ChomskyNF g = chomskyNF $ cykTest 50
-- >>> cfgSize g
-- 4028
main :: IO ()
main = do
    [nStr] <- getArgs
    let n = read nStr
    print $ cyk (cykTest n) (replicate (n+1) 'a')
