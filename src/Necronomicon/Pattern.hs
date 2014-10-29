module Necronomicon.Pattern where

import Prelude
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as THS
import Language.Haskell.TH.Quote

import Data.Typeable
import Data.Data
import Data.Tree 

data Pattern = DoublePattern Double
             | RestPattern
             | TreePattern [Pattern]
             | ListPattern [Pattern]
             deriving (Show,Typeable,Data)

data StringTree = EmptyStringList
                | StringValue String
                | StringList [String]
                | StringTree [StringTree]
                deriving (Show,Typeable,Data)

pseq = QuasiQuoter{quoteExp = parsePattern}

parsePattern s = dataToExpQ (const Nothing) (p s)
    where
        p = stringTreeToPattern . toStringTree

insertStringList :: StringTree -> StringTree -> StringTree
insertStringList s (EmptyStringList) = s
insertStringList (StringValue sv1) (StringValue sv2)    = StringValue $ sv2++sv1
insertStringList sl@(StringTree _)   sv@(StringValue _) = StringTree $ sv:sl:[]
insertStringList sv@(StringValue _)    (StringTree ss)  = StringTree $ init ss ++ [(insertStringList sv $ last ss)]
insertStringList sl@(StringTree _)   (StringTree sl2)   = StringTree $ sl2++[sl]

toStringList :: StringTree -> StringTree
toStringList (StringValue v)   = StringTree $ [StringValue v]
toStringList (EmptyStringList) = StringTree []
toStringList sl@(StringTree _) = sl

toStringTree :: String -> StringTree
toStringTree s = splitWords . fst $ go EmptyStringList s
    where
        go st []       = (st,[])
        go st ('[':ss) = (StringTree [st,toStringList st',leftOverst],lo')
            where
                (st',leftOver)  = go EmptyStringList ss
                (leftOverst,lo') = go EmptyStringList leftOver
        go st (']':ss) = (st,ss)
        go st (s:ss)   = go (insertStringList (StringValue $ s:[]) st) ss

splitWords :: StringTree -> StringTree
splitWords (EmptyStringList) = EmptyStringList
splitWords (StringValue v)   = StringTree $ map StringValue $ words v
splitWords (StringTree v)    = StringTree $ foldr collapseList [] v
    where
        collapseList (StringValue v) a = (map StringValue $ words v) ++ a
        collapseList (StringTree  v) a = splitWords (StringTree v) : a
        collapseList (EmptyStringList) a = a

stringTreeToPattern :: StringTree -> Pattern
stringTreeToPattern (EmptyStringList) = RestPattern
stringTreeToPattern (StringValue v)   = stringToPattern v
stringTreeToPattern (StringTree v)    = ListPattern $ map stringTreeToPattern v

stringToPattern :: String -> Pattern 
stringToPattern s =
    case s of
        ('_':[])   -> RestPattern
        _          -> DoublePattern (read s ::Double)


        
