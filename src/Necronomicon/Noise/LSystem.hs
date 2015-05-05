module Necronomicon.Noise.LSystem where

import qualified Data.Set as S
import qualified Data.Map as M

newtype Alphabet = Alphabet (S.Set Char) deriving (Show, Eq)
newtype RuleSet = RuleSet (M.Map Char String) deriving (Show, Eq)
data LSystem = LSystem Alphabet RuleSet deriving (Show, Eq)

lsystemGeneration :: LSystem -> String -> Int -> String
lsystemGeneration (LSystem _ (RuleSet ruleset)) axiom n
    | n <= 0 = axiom
    | otherwise = foldl prGeneration axiom ([1 .. n] :: [Int])
    where
        prGeneration alphabet' _ = foldl expand [] alphabet'
        expand acc letter = case M.lookup letter ruleset of
                    Nothing -> acc ++ [letter]
                    Just ruleString  -> acc ++ ruleString

mkAlphabet :: String -> Alphabet
mkAlphabet = Alphabet . S.fromList

mkRuleSet :: [(Char, String)] -> RuleSet
mkRuleSet = RuleSet . M.fromList

lsystemFromRuleSet :: RuleSet -> LSystem
lsystemFromRuleSet r@(RuleSet ruleset) = LSystem (Alphabet alphabet) r
    where
        alphabet = S.fromList . map fst $ M.toList ruleset

algaeSystem :: Int -> String
algaeSystem = lsystemGeneration lsystem "A"
    where
        lsystem = lsystemFromRuleSet $ mkRuleSet [
                ('A', "AB"),
                ('B', "A")
            ]

pythagorasTree :: Int -> String
pythagorasTree = lsystemGeneration lsystem "0"
    where
        lsystem = lsystemFromRuleSet $ mkRuleSet [
                ('1', "11"),
                ('0', "1[0]0")
            ]

cantorDust :: Int -> String
cantorDust = lsystemGeneration lsystem "A"
    where
        lsystem = lsystemFromRuleSet $ mkRuleSet [
                ('A', "ABA"),
                ('B', "BBB")
            ]

kochCurve :: Int -> String
kochCurve = lsystemGeneration lsystem "F"
    where
        lsystem = lsystemFromRuleSet $ mkRuleSet [
                ('F', "F+F-F-F+F")
            ]

sierpinskiTriangle :: Int -> String
sierpinskiTriangle = lsystemGeneration lsystem "A"
    where
        lsystem = lsystemFromRuleSet $ mkRuleSet [
                ('A', "B-A-B"),
                ('B', "A+B+A")
            ]

dragonCurve :: Int -> String
dragonCurve = lsystemGeneration lsystem "FX"
    where
        lsystem = lsystemFromRuleSet $ mkRuleSet [
                ('X', "X+YF+"),
                ('Y', "-FX-Y")
            ]

fractalPlant :: Int -> String
fractalPlant = lsystemGeneration lsystem "X"
    where
        lsystem = lsystemFromRuleSet $ mkRuleSet [
                ('X', "F-[[X]+X]+F[+FX]-X"),
                ('F', "FF")
            ]
