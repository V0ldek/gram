module CFG.Parser (
    cfg,
    fullParseCfg
) where

import CFG.Data
import Text.Parsec
import Data.Char

type Parser = Parsec String ()

-- |
-- Parsing
-- >>> parse cfg "" "A -> _ | AA | (A)"
-- Right A -> _ | AA | (A)
--
-- >>> parse cfg "" "A -> _ | (A) | AA | abcd"
-- Right A -> _ | (A) | AA | abcd
--
-- >>> parse cfg "" "A -> _\nA -> (A)\nA -> AA"
-- Right A -> _
-- A -> (A)
-- A -> AA
--
-- >>> parse cfg "" "A -> AB | B\nB -> b"
-- Right A -> AB | B
-- B -> b
--
-- >>> parse cfg "" "A -> AB\nA -> B\nB -> b"
-- Right A -> AB
-- A -> B
-- B -> b
--
-- >>> parse cfg "" "A -> \\_ | (A) | AA"
-- Right A -> \_ | (A) | AA
--
-- >>> parse cfg "" "A -> aA | B\nB -> b | _\n"
-- Right A -> aA | B
-- B -> b | _
cfg :: Parser Grammar
cfg = Grammar <$> sepEndBy1 rule newline

rule :: Parser Rule
rule = do
    spaces
    nt <- nonterminal
    _ <- spaces >> string "->" >> spaces
    derivations <- derivation `sepBy1` try (spaces >> string "|" >> spaces)
    return $ Rule nt derivations


derivation :: Parser Derivation
derivation = do
    derivation <- many1 symbol
    return $ Der $ case derivation of
        [ST eps] | eps == derEpsilon -> []
        _ -> map clearEscape derivation
    where
        clearEscape (ST ('\\':ts)) = ST ts
        clearEscape x              = x 

symbol :: Parser Symbol
symbol = choice [SNT <$> nonterminal, ST <$> terminal]

nonterminal :: Parser Nonterminal
nonterminal = do
    x <- upper
    xs <- many digit
    return (x:xs)

terminal :: Parser Terminal
terminal = many1 $ satisfy (\c -> not (isSpace c) && not (isUpper c) && c /= '|')

-- |
-- Full input parsing
-- >>> fullParseCfg ("", 0, 0) "\nA -> aA | B\nB -> b | _\n"
-- Grammar [Rule "A" [Der [ST "a",SNT "A"],Der [SNT "B"]],Rule "B" [Der [ST "b"],Der []]]
--
fullParseCfg :: (Monad m, MonadFail m) => (String, Int, Int) -> String -> m Grammar
fullParseCfg (file, line, col) s =
    case runParser go () "" s of
        Left err -> fail $ show err
        Right g  -> return g
    where
        go = do
            pos <- getPosition
            setPosition $
                flip setSourceName file $
                flip setSourceLine line $
                setSourceColumn pos col
            spaces
            g <- cfg
            spaces
            eof 
            return g
