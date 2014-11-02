module Necronomicon.Language.Layout
       (NotePattern(NotePatternValue,NoteRestPattern,NoteChordPattern,NotePatternList),
        SynthPattern(SynthPatternValue,SynthRestPattern,SynthChordPattern,SynthPatternList),
        l)
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

-- modifiers???? How to do this in new layout DSL?
-- Negative numbers....
--combine beat patterns into same DSL?
--plays at last value, 

---------------
-- Data types
---------------

-- | The data structure used to represent patterns in Necronomicon
data NotePattern = NotePatternValue  Double
                 | NoteRestPattern
                 | NoteChordPattern [Double]
                 | NotePatternList  [NotePattern] Int
                 deriving (Show)

data SynthPattern = SynthPatternValue  String
                  | SynthRestPattern
                  | SynthChordPattern [String]
                  | SynthPatternList  [SynthPattern] Int
                  deriving (Show)

-- Pattern

-- | The data structure used for internal syntax parsing of patterns from quasi-quotations in layout form.
data ParsecPattern = NoteParsec        Double
                   | NoteRestParsec
                   | NoteChordParsec  [Double]
                   | NoteParsecList [ParsecPattern]
                     
                   | SynthParsec       String
                   | SynthRestParsec
                   | SynthChordParsec [String]
                   | SynthParsecList [ParsecPattern]


                   | ErrorParsec String
                   deriving (Show)


                   -- | AtomParsecPattern String


----------------------------------
-- Parsing with quasi-quotation
----------------------------------

-- |The quasi-quoter used to generate patterns from quasi-quotations in layout form.
l = QuasiQuoter{quoteExp = parsecPatternToQExpr . parseParsecPattern }

-- | Parses a string into a ParsecPattern structure
parseParsecPattern input =
    case parse parseExpr "pattern" (('[':input)++[']']) of
        Left  err -> ErrorParsec $ show err
        Right val -> val

-- | The main paring combinator of the layout pattern DSL
parseExpr :: Parser ParsecPattern
parseExpr = try synthPattern <|> notePattern

synthPattern :: Parser ParsecPattern
synthPattern = parseSynthArray <|> parseSynthChordTuples <|> parseSynthRest <|> parseSynthPattern

notePattern :: Parser ParsecPattern
notePattern = parseArray <|> parseChordTuples <|> parseRest <|> parseNumber <|> synthPattern

-- <|> parseAtom

-- | 
parseRest :: Parser ParsecPattern
parseRest = return NoteRestParsec <* char '_'

parseRawAtom :: Parser String
parseRawAtom = (:) <$> letter <*> many (letter <|> digit)
            
-- parseAtom :: Parser ParsecPattern
-- parseAtom = AtomParsecPattern <$> parseRawAtom

parseRawNumber :: Parser Double
parseRawNumber = do
    first <- optionMaybe $ (char '-')
    spaces
    d <- many1 digit
    case first of
        Just f  -> return . read $ f:d
        Nothing -> return . read $ d
        
parseNumber :: Parser ParsecPattern
parseNumber = NoteParsec <$> parseRawNumber

parseArray :: Parser ParsecPattern
parseArray = NoteParsecList <$> (char '[' *> spaces *> sepEndBy notePattern spaces <* char ']')

parseChordTuples :: Parser ParsecPattern
parseChordTuples = NoteChordParsec <$> (between (char '(' *> spaces) (spaces *> char ')') . sepBy parseRawNumber $ try $ spaces *> char ',')


-----------------

parseSynthArray :: Parser ParsecPattern
parseSynthArray = SynthParsecList <$> (char '[' *> spaces *> sepEndBy synthPattern spaces <* char ']')

parseSynthRest :: Parser ParsecPattern
parseSynthRest = return SynthRestParsec <* char '_'

parseSynthChordTuples :: Parser ParsecPattern
parseSynthChordTuples = SynthChordParsec <$> (between (char '(' *> spaces) (spaces *> char ')') . sepBy parseRawAtom $ try $ spaces *> char ',')

parseSynthPattern :: Parser ParsecPattern
parseSynthPattern = SynthParsec <$> parseRawAtom
        

---------------------
-- convert to QExpr
--------------------

parsecPatternToQExpr :: ParsecPattern -> Q Exp

parsecPatternToQExpr (NoteParsec d) = do
    name <- getName "NotePatternValue"
    return $ AppE (ConE name) (LitE . RationalL $ toRational d)

parsecPatternToQExpr NoteRestParsec = do
    name <- getName "NoteRestPattern"
    return $ ConE name

parsecPatternToQExpr (NoteParsecList ps) = do
    name <- getName "NotePatternList"
    list <- sequence $ map parsecPatternToQExpr ps
    return $ AppE (AppE (ConE name) (ListE list)) (LitE . IntegerL $ fromIntegral $ length ps)

parsecPatternToQExpr (NoteChordParsec ds) = do
    name <- getName "NoteChordPattern"
    return $ AppE (ConE name) (ListE $ map (LitE . RationalL . toRational) ds)



parsecPatternToQExpr (SynthParsec s) = do
    name <- getName "SynthPatternValue"
    return $ AppE (ConE name) (LitE $ StringL s)

parsecPatternToQExpr SynthRestParsec = do
    name <- getName "SynthRestPattern"
    return $ ConE name

parsecPatternToQExpr (SynthParsecList ps) = do
    name <- getName "SynthPatternList"
    list <- sequence $ map parsecPatternToQExpr ps
    return $ AppE (AppE (ConE name) (ListE list)) (LitE . IntegerL $ fromIntegral $ length ps)

parsecPatternToQExpr (SynthChordParsec ds) = do
    name <- getName "SynthChordPattern"
    return $ AppE (ConE name) (ListE $ map (LitE . StringL) ds)

-- parsecPatternToQExpr (AtomParsecPattern a) = do
    -- name <- lookupValueName a
    -- let name' = case name of
            -- Just n  -> n
            -- Nothing -> mkName a
    -- return $ VarE name'

parsecPatternToQExpr (ErrorParsec e) = fail e

getName :: String -> Q Name
getName s = do
    name <- lookupTypeName s
    return $ case name of
        Just n  -> n
        Nothing -> mkName s

