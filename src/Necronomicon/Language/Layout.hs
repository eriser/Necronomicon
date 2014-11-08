module Necronomicon.Language.Layout
       -- (Pattern(PatternValue,PatternRest,PatternChord,PatternList),
        (lich,toValueDuration)
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
-- data Pattern a = PatternValue a
               -- | PatternRest
               -- | PatternChord [a]
               -- | PatternList [Pattern a] Int
               -- deriving (Show)

-- | The data structure used for internal syntax parsing of patterns from quasi-quotations in layout form.
data ParsecPattern a = ParsecValue a
                     | ParsecRest
                     | ParsecChord [a]
                     | ParsecList [ParsecPattern a]
                     deriving (Show,Typeable,Data)


type Time = Double
data Notation a = Note a | Rest | Chord [a] | TimedNote (Notation a) Time

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
        Right val -> do
            name <- getValueName "toValueDuration"
            v <- val
            return $ AppE (VarE name) v

-- | The main paring combinator of the layout pattern DSL
parseExpr :: Parser (Q Exp)
-- parseExpr = (parsecPatternToQExpr <$> try synthPattern) <|> (parsecPatternToQExpr <$> try functionPattern) <|> (parsecPatternToQExpr <$> notePattern)
parseExpr = (dataToExpQ (const Nothing) <$> try synthPattern) <|> (dataToExpQ (const Nothing) <$> try functionPattern) <|> (dataToExpQ (const Nothing) <$> notePattern)

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

-- class ParsecPatternExpression a where
    -- parsecPatternToQExpr :: a -> Q Exp

-- instance ParsecPatternExpression (ParsecPattern Double) where
    -- parsecPatternToQExpr (ParsecValue d) = do
        -- name <- getName "Note"
        -- name' <- getName "Necronomicon.Patterns.Leaf" 
        -- return $ AppE (ConE name') $ AppE (ConE name) (LitE . RationalL $ toRational d)

    -- parsecPatternToQExpr ParsecRest = do
        -- name <- getName "Rest"
        -- name' <- getName "Necronomicon.Patterns.Leaf" 
        -- return $ AppE (ConE name') $ ConE name

    -- parsecPatternToQExpr (ParsecList ps) = do
        -- name <- getName "Necronomicon.Patterns.Node"
        -- list <- sequence $ map parsecPatternToQExpr ps
        -- return $ AppE (AppE (ConE name) (ListE list)) (LitE . IntegerL $ fromIntegral $ length ps)

    -- parsecPatternToQExpr (ParsecChord ds) = do
        -- name <- getName "Chord"
        -- name' <- getName "Necronomicon.Patterns.Leaf" 
        -- return $ AppE (ConE name) (ListE $ map (LitE . RationalL . toRational) ds)

    -- parsecPatternToQExpr (ErrorParsec e) = fail e

-- instance ParsecPatternExpression (ParsecPattern String) where
    -- parsecPatternToQExpr (ParsecValue s) = do
        -- name <- getName "Note"
        -- name' <- getName "Necronomicon.Patterns.Leaf" 
        -- return $ AppE (ConE name') $ AppE (ConE name) (LitE $ StringL s)

    -- parsecPatternToQExpr ParsecRest = do
        -- name <- getName "Rest"
        -- name' <- getName "Necronomicon.Patterns.Leaf" 
        -- return $ AppE (ConE name') $ ConE name

    -- parsecPatternToQExpr (ParsecList ps) = do
        -- name <- getName "Necronomicon.Patterns.Node"
        -- list <- sequence $ map parsecPatternToQExpr ps
        -- return $ AppE (AppE (ConE name) (ListE list)) (LitE . IntegerL $ fromIntegral $ length ps)

    -- parsecPatternToQExpr (ParsecChord ds) = do
        -- name <- getName "Chord"
        -- name' <- getName "Necronomicon.Patterns.Leaf" 
        -- return $ AppE (ConE name') $ AppE (ConE name) (ListE $ map (LitE . StringL) ds)

    -- parsecPatternToQExpr (ErrorParsec e) = fail e

-- instance ParsecPatternExpression (ParsecPattern Exp) where
    -- parsecPatternToQExpr (ParsecValue s) = do
        -- name <- getName "Note"
        -- name' <- getName "Necronomicon.Patterns.Leaf" 
        -- return $ AppE (ConE name') $ AppE (ConE name) s

    -- parsecPatternToQExpr (ParsecList ps) = do
        -- name <- getName "Necronomicon.Patterns.Node"
        -- list <- sequence $ map parsecPatternToQExpr ps
        -- return $ AppE (AppE (ConE name) (ListE list)) (LitE . IntegerL $ fromIntegral $ length ps)

    -- parsecPatternToQExpr (ParsecChord fs) = do
        -- name <- getName "Chord"
        -- name' <- getName "Necronomicon.Patterns.Leaf" 
        -- return $ AppE (ConE name') $ AppE (ConE name) (ListE fs)

    -- parsecPatternToQExpr (ErrorParsec e) = fail e

-- toPTree = NP.ptree . NP.PVal


-- parsecPatternToQExpr (AtomParsecPattern a) = do
    -- name <- lookupValueName a
    -- let name' = case name of
            -- Just n  -> n
            -- Nothing -> mkName a
    -- return $ VarE name'


toValueDuration :: ParsecPattern a -> NP.PList (NP.Pattern a,Double,Time)
toValueDuration (ParsecValue a) = NP.PVal [(NP.PVal a,1,0)]
toValueDuration  ParsecRest     = NP.PVal [(NP.PNothing,1,0)]
toValueDuration (ParsecList as) = NP.PVal $ map (\(v,d,t) -> (v,d,totalTime - t)) $ foldr countTime [] withoutTime 
    where
        totalTime   = foldr (\(_,d) d2 -> d + d2) 0 withoutTime
        withoutTime = foldr (go 1) [] as
        countTime (v1,d1) []               = (v1,d1,d1) : []
        countTime (v1,d1) ((v2,d2,t) : vs) = (v1,d1,(d2+t)) : (v2,d2,t) : vs 
        go d (ParsecValue a) vs = (NP.PVal a,d)   : vs
        go d  ParsecRest     vs = (NP.PNothing,d) : vs
        go d (ParsecList as) vs = foldr (go (d / (fromIntegral $ length as))) vs as


getName :: String -> Q Name
getName s = do
    name <- lookupTypeName s
    return $ case name of
        Just n  -> n
        Nothing -> mkName s

getValueName :: String -> Q Name
getValueName s = do
    name <- lookupValueName s
    return $ case name of
        Just n  -> n
        Nothing -> mkName s


-- toTree :: Pattern a -> NP.Tree (Notation a)
-- toTree (PatternValue v)   = NP.Leaf (Note v)
-- toTree (PatternList ps l) = NP.Node (map toTree ps) l
-- toTree PatternRest        = NP.Leaf Rest
-- toTree (PatternChord ps)  = NP.Leaf (Chord ps)
