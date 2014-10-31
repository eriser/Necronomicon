module Necronomicon.Pattern where

import Prelude
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Control.Monad
import Text.ParserCombinators.Parsec
import System.Environment

import Data.Typeable
import Data.Data
import Data.Tree 

-- modifiers???? How to do this in new layout DSL?
-- Negative numbers....

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

-- |The quasi-quoter used to generate patterns from quasi-quotations in layout form.
ps = QuasiQuoter{quoteExp = parsecPatternToQExpr . parseParsecPattern }

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
parseRest = do
    s <- char '_'
    return RestParsecPattern
        
parseAtom :: Parser ParsecPattern
parseAtom = do 
    first <- letter
    rest  <- many (letter <|> digit)
    let atom = first:rest
    return $ AtomParsecPattern atom

parseNumber :: Parser ParsecPattern
parseNumber = do
    d <- many1 digit
    return . DoubleParsecPattern $ read d

parseArray :: Parser ParsecPattern
parseArray = do
    char '['
    spaces
    x <- sepEndBy parseExpr spaces
    char ']'
    return $ ListParsecPattern x

parseChordTuples :: Parser ParsecPattern
parseChordTuples = do
    char '('
    spaces
    x <- sepBy (many1 digit) (spaces >> char ',' >> spaces)
    char ')'
    return $ ChordParsecPattern (map read x)

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

