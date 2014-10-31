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

data Pattern = DoublePattern Double
             | RestPattern
             | ListPattern [Pattern]
             deriving (Show,Typeable,Data)

data ParsecPattern = DoubleParsecPattern Double
                   | RestParsecPattern
                   | ListParsecPattern [ParsecPattern]
                   | AtomParsecPattern String
                   | ErrorParsecPattern String
                   deriving (Show,Typeable,Data)

ps = QuasiQuoter{quoteExp = parsecPatternToQExpr . parseParsecPattern }

parseParsecPattern input =
    case parse parseExpr "pattern" (('[':input)++[']']) of
        Left  err -> ErrorParsecPattern $ show err
        Right val -> val

parseExpr :: Parser ParsecPattern
parseExpr = parseArray
        <|> parseRest
        <|> parseNumber
        <|> parseAtom

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

parsecPatternToQExpr (AtomParsecPattern a) = do
    name <- lookupValueName a
    let name' = case name of
            Just n  -> n
            Nothing -> mkName a
    return $ VarE name'

getName :: String -> Q Name
getName s = do
    name <- lookupTypeName s
    return $ case name of
        Just n  -> n
        Nothing -> mkName s

