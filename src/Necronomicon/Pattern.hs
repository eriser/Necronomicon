module Necronomicon.Pattern where

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

-- modifiers???? How to do this in new layout DSL?
-- Negative numbers....
--combine beat patterns into same DSL?
--plays at last value, 

---------------
-- Data types
---------------

-- | The data structure used to represent patterns in Necronomicon
data Pattern = DoublePattern Double
             | RestPattern
             | ChordPattern [Double]
             | ListPattern [Pattern]
             deriving (Show,Typeable,Data)

-- | The data structure used for internal syntax parsing of patterns from quasi-quotations in layout form.
data ParsecPattern = DoubleParsecPattern Double
                   | RestParsecPattern
                   | ChordParsecPattern [Double]
                   | ListParsecPattern [ParsecPattern]
                   | AtomParsecPattern String
                   | ErrorParsecPattern String
                   deriving (Show,Typeable,Data)

----------------------------------
-- Parsing with quasi-quotation
----------------------------------

-- |The quasi-quoter used to generate patterns from quasi-quotations in layout form.
ps = QuasiQuoter { quoteExp = parsecPatternToQExpr . parseParsecPattern }

-- | Parses a string into a ParsecPattern structure
parseParsecPattern input =
    case parse parseExpr "pattern" (('[':input)++[']']) of
        Left  err -> ErrorParsecPattern $ show err
        Right val -> val

-- | The main paring combinator of the layout pattern DSL
parseExpr :: Parser ParsecPattern
parseExpr = parseArray
        <|> parseChordTuples
        <|> parseRest
        <|> parseNumber
        <|> parseAtom

-- | 
parseRest :: Parser ParsecPattern
parseRest = return RestParsecPattern <* char '_'
        
parseAtom :: Parser ParsecPattern
parseAtom = AtomParsecPattern <$> ((:) <$> letter <*> many (letter <|> digit))

parseRawNumber :: Parser Double
parseRawNumber = do
    first <- optionMaybe $ (char '-')
    spaces
    d <- many1 digit
    case first of
        Just f  -> return . read $ f:d
        Nothing -> return . read $ d
        
parseNumber :: Parser ParsecPattern
parseNumber = DoubleParsecPattern <$> parseRawNumber

parseArray :: Parser ParsecPattern
parseArray = ListParsecPattern <$> (char '[' *> spaces *> sepEndBy parseExpr spaces <* char ']')

parseChordTuples :: Parser ParsecPattern
parseChordTuples = ChordParsecPattern <$> (between (char '(' *> spaces) (spaces *> char ')') . sepBy parseRawNumber $ try $ spaces *> char ',')

---------------------
-- convert to QExpr
--------------------

parsecPatternToQExpr :: ParsecPattern -> Q Exp

parsecPatternToQExpr (DoubleParsecPattern d) = do
    name <- getName "DoublePattern"
    return $ AppE (ConE name) (LitE . RationalL $ toRational d)

parsecPatternToQExpr RestParsecPattern = do
    name <- getName "RestPattern"
    return $ ConE name

parsecPatternToQExpr (ListParsecPattern ps) = do
    name <- getName "ListPattern"
    list <- sequence $ map parsecPatternToQExpr ps
    return $ AppE (ConE name) (ListE list)

parsecPatternToQExpr (ChordParsecPattern ds) = do
    name <- getName "ChordPattern"
    return $ AppE (ConE name) (ListE $ map (LitE . RationalL . toRational) ds)

parsecPatternToQExpr (AtomParsecPattern a) = do
    name <- lookupValueName a
    let name' = case name of
            Just n  -> n
            Nothing -> mkName a
    return $ VarE name'

parsecPatternToQExpr (ErrorParsecPattern e) = fail e

getName :: String -> Q Name
getName s = do
    name <- lookupTypeName s
    return $ case name of
        Just n  -> n
        Nothing -> mkName s

