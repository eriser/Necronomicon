module Necronomicon.Language.Layout
       (Pattern(PatternValue,PatternRest,PatternChord,PatternList),
        lich, toTree)
       where

import Prelude
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Control.Monad
import Control.Applicative ((<*),(<*>),(*>),some,(<$>),liftA)
import Text.ParserCombinators.Parsec
import System.Environment

import Data.Typeable
import Data.Data
import Data.Tree 

import qualified Necronomicon.Patterns as NP

-- modifiers???? How to do this in new layout DSL?
--list syntactic sugar?

---------------
-- Data types
---------------

-- | Generic pattern container type
data Pattern a = PatternValue a
               | PatternRest
               | PatternChord [a]
               | PatternList [Pattern a] Int
               deriving (Show)

-- | The data structure used for internal syntax parsing of patterns from quasi-quotations in layout form.
data ParsecPattern a = ParsecValue a
                     | ParsecRest
                     | ParsecChord [a]
                     | ParsecList [ParsecPattern a]
                     | ErrorParsec String
                     deriving (Show)


data Notation a = Note a | Rest | Chord [a]

instance (Show a) => Show (Notation a) where
    show (Note a) = "(Note " ++ (show a) ++ ")"
    show Rest = "Rest"
    show (Chord as) = "(Chord " ++ (show as) ++ ")"

instance Show (a -> a) where
    show _ = "(a -> a)"

----------------------------------
-- Parsing with quasi-quotation
----------------------------------

-- |The quasi-quoter used to generate patterns from quasi-quotations in layout form.
lich = QuasiQuoter{quoteExp =  parseParsecPattern }

-- | Parses a string into a ParsecPattern structure
parseParsecPattern input =
    case parse parseExpr "pattern" (('[':input)++[']']) of
        Left  err -> fail $ show err
        Right val -> val

-- | The main paring combinator of the layout pattern DSL
parseExpr :: Parser (Q Exp)
parseExpr = (parsecPatternToQExpr <$> try synthPattern) <|> (parsecPatternToQExpr <$> try functionPattern) <|> (parsecPatternToQExpr <$> notePattern)

synthPattern :: Parser (ParsecPattern String)
synthPattern = parseSynthArray <|> parseSynthChordTuples <|> parseSynthRest <|> parseSynthPattern

functionPattern :: Parser (ParsecPattern Exp)
functionPattern = parseFunctionArray <|> (try parseFunctionPattern <|> parseFunctionChordTuples) <|> parseFunctionRest

notePattern :: Parser (ParsecPattern Double)
notePattern = parseArray <|> parseChordTuples <|> parseRest <|> parseNumber

------------------------
-- NotePattern parsing
------------------------

parseRest :: Parser (ParsecPattern Double)
parseRest = return ParsecRest <* char '_'

parseRawAtom :: Parser String
parseRawAtom = (:) <$> letter <*> many (letter <|> digit)
            
parseRawNumber :: Parser Double
parseRawNumber = do
    first <- optionMaybe $ (char '-')
    spaces
    d <- many1 digit
    case first of
        Just f  -> return . read $ f:d
        Nothing -> return . read $ d
        
parseNumber :: Parser (ParsecPattern Double)
parseNumber = ParsecValue <$> parseRawNumber

parseArray :: Parser (ParsecPattern Double)
parseArray = ParsecList <$> (char '[' *> spaces *> sepEndBy1 notePattern spaces <* char ']')

parseChordTuples :: Parser (ParsecPattern Double)
parseChordTuples = ParsecChord <$> (between (char '(' *> spaces) (spaces *> char ')') . sepBy1 parseRawNumber $ try $ spaces *> char ',')

------------------------
-- SynthPattern parsing
------------------------

parseSynthArray :: Parser (ParsecPattern String)
parseSynthArray = ParsecList <$> (char '[' *> spaces *> sepEndBy1 synthPattern spaces <* char ']')

parseSynthRest :: Parser (ParsecPattern String)
parseSynthRest = return ParsecRest <* char '_'

parseSynthChordTuples :: Parser (ParsecPattern String)
parseSynthChordTuples = ParsecChord <$> (between (char '(' *> spaces) (spaces *> char ')') . sepBy1 parseRawAtom $ try $ spaces *> char ',')

parseSynthPattern :: Parser (ParsecPattern String)
parseSynthPattern = ParsecValue <$> parseRawAtom
        

---------------------------
-- FunctionPattern parsing
---------------------------

parseFunctionArray :: Parser (ParsecPattern Exp)
parseFunctionArray = ParsecList <$> (char '[' *> spaces *> sepEndBy1 functionPattern spaces <* char ']')

parseFunctionRest :: Parser (ParsecPattern Exp)
parseFunctionRest = return (ParsecValue (VarE (mkName "Prelude.id"))) <* char '_'

parseFunctionChordTuples :: Parser (ParsecPattern Exp)
parseFunctionChordTuples = ParsecChord <$> (between (char '(' *> spaces) (spaces *> char ')') . sepBy1 parseRawFunction $ try $ spaces *> char ',')

parseFunctionPattern :: Parser (ParsecPattern Exp)
parseFunctionPattern = ParsecValue <$> parseRawFunction

parseRawFunction :: Parser Exp
parseRawFunction = between (char '(' *> spaces) (spaces *> char ')') (try leftSection <|> rightSection)
    where
        leftSection  = do
            f <- oneOf "+*/"
            spaces
            v <- parseRawNumber
            return $ case f of
                '+' -> InfixE Nothing (VarE (mkName "Prelude.+")) (Just (LitE (RationalL $ toRational v)))
                -- '-' -> AppE (VarE (mkName "Prelude.subtract")) (LitE (RationalL $ toRational v))
                '*' -> InfixE Nothing (VarE (mkName "Prelude.*")) (Just (LitE (RationalL $ toRational v)))
                '/' -> InfixE Nothing (VarE (mkName "Prelude./")) (Just (LitE (RationalL $ toRational v)))
        rightSection  = do
            v <- parseRawNumber
            spaces
            f <- oneOf "+-*/"
            return $ case f of
                '+' -> InfixE (Just (LitE (RationalL $ toRational v))) (VarE (mkName "Prelude.+")) Nothing
                '-' -> InfixE (Just (LitE (RationalL $ toRational v))) (VarE (mkName "Prelude.-")) Nothing
                '*' -> InfixE (Just (LitE (RationalL $ toRational v))) (VarE (mkName "Prelude.*")) Nothing
                '/' -> InfixE (Just (LitE (RationalL $ toRational v))) (VarE (mkName "Prelude./")) Nothing

---------------------
-- convert to QExpr
--------------------

class ParsecPatternExpression a where
    parsecPatternToQExpr :: a -> Q Exp

instance ParsecPatternExpression (ParsecPattern Double) where
    parsecPatternToQExpr (ParsecValue d) = do
        name <- getName "PatternValue"
        return $ AppE (ConE name) (LitE . RationalL $ toRational d)

    parsecPatternToQExpr ParsecRest = do
        name <- getName "PatternRest"
        return $ ConE name

    parsecPatternToQExpr (ParsecList ps) = do
        name <- getName "PatternList"
        list <- sequence $ map parsecPatternToQExpr ps
        return $ AppE (AppE (ConE name) (ListE list)) (LitE . IntegerL $ fromIntegral $ length ps)

    parsecPatternToQExpr (ParsecChord ds) = do
        name <- getName "PatternChord"
        return $ AppE (ConE name) (ListE $ map (LitE . RationalL . toRational) ds)

    parsecPatternToQExpr (ErrorParsec e) = fail e

instance ParsecPatternExpression (ParsecPattern String) where
    parsecPatternToQExpr (ParsecValue s) = do
        name <- getName "PatternValue"
        return $ AppE (ConE name) (LitE $ StringL s)

    parsecPatternToQExpr ParsecRest = do
        name <- getName "PatternRest"
        return $ ConE name

    parsecPatternToQExpr (ParsecList ps) = do
        name <- getName "PatternList"
        list <- sequence $ map parsecPatternToQExpr ps
        return $ AppE (AppE (ConE name) (ListE list)) (LitE . IntegerL $ fromIntegral $ length ps)

    parsecPatternToQExpr (ParsecChord ds) = do
        name <- getName "PatternChord"
        return $ AppE (ConE name) (ListE $ map (LitE . StringL) ds)

    parsecPatternToQExpr (ErrorParsec e) = fail e

instance ParsecPatternExpression (ParsecPattern Exp) where
    parsecPatternToQExpr (ParsecValue s) = do
        name <- getName "PatternValue"
        return $ AppE (ConE name) s

    parsecPatternToQExpr (ParsecList ps) = do
        name <- getName "PatternList"
        list <- sequence $ map parsecPatternToQExpr ps
        return $ AppE (AppE (ConE name) (ListE list)) (LitE . IntegerL $ fromIntegral $ length ps)

    parsecPatternToQExpr (ParsecChord fs) = do
        name <- getName "PatternChord"
        return $ AppE (ConE name) (ListE fs)

    parsecPatternToQExpr (ErrorParsec e) = fail e


-- parsecPatternToQExpr (AtomParsecPattern a) = do
    -- name <- lookupValueName a
    -- let name' = case name of
            -- Just n  -> n
            -- Nothing -> mkName a
    -- return $ VarE name'


getName :: String -> Q Name
getName s = do
    name <- lookupTypeName s
    return $ case name of
        Just n  -> n
        Nothing -> mkName s

toTree :: Pattern a -> NP.Tree (Notation a)
toTree (PatternValue v)   = NP.Leaf (Note v)
toTree (PatternList ps l) = NP.Node (map toTree ps) l
toTree PatternRest = NP.Leaf Rest
toTree (PatternChord ps) = NP.Leaf (Chord ps)
