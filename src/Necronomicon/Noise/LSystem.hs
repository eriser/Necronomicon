module Necronomicon.Noise.LSystem where

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Necronomicon.Util.Grid as G

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

-- creates an infinite list of strings containing all possible versions of a given lsystem
lsystemGenerations :: LSystem -> String -> [String]
lsystemGenerations (LSystem _ (RuleSet ruleset)) axiom = lSystemGenerationsList
    where
        indexes = [0..] :: [Int]
        lSystemGenerationsList = axiom : map (\n -> prGeneration (lSystemGenerationsList !! n)) indexes
        prGeneration alphabet' = foldl expand [] alphabet'
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

mkLSystemGenerationsGrid :: [String] -> G.Grid Char
mkLSystemGenerationsGrid generations = G.Grid $ V.fromList $ map (V.fromList) generations

algaeSystem :: Int -> String
algaeSystem = lsystemGeneration lsystem "A"
    where
        lsystem = lsystemFromRuleSet $ mkRuleSet [
                ('A', "AB"),
                ('B', "A")
            ]

algaeSystems :: [String]
algaeSystems = lsystemGenerations lsystem "A"
    where
        lsystem = lsystemFromRuleSet $ mkRuleSet [
                ('A', "AB"),
                ('B', "A")
            ]

algaeSystemGrid :: Int -> G.Grid Char
algaeSystemGrid numGenerations = mkLSystemGenerationsGrid $ take numGenerations algaeSystems

pythagorasTree :: Int -> String
pythagorasTree = lsystemGeneration lsystem "0"
    where
        lsystem = lsystemFromRuleSet $ mkRuleSet [
                ('1', "11"),
                ('0', "1[0]0")
            ]

pythagorasTrees :: [String]
pythagorasTrees = lsystemGenerations lsystem "0"
    where
        lsystem = lsystemFromRuleSet $ mkRuleSet [
                ('1', "11"),
                ('0', "1[0]0")
            ]

pythagorasTreeGrid :: Int -> G.Grid Char
pythagorasTreeGrid numGenerations = mkLSystemGenerationsGrid $ take numGenerations pythagorasTrees

cantorDust :: Int -> String
cantorDust = lsystemGeneration lsystem "A"
    where
        lsystem = lsystemFromRuleSet $ mkRuleSet [
                ('A', "ABA"),
                ('B', "BBB")
            ]

cantorDusts :: [String]
cantorDusts = lsystemGenerations lsystem "A"
    where
        lsystem = lsystemFromRuleSet $ mkRuleSet [
                ('A', "ABA"),
                ('B', "BBB")
            ]

cantorDustGrid :: Int -> G.Grid Char
cantorDustGrid numGenerations = mkLSystemGenerationsGrid $ take numGenerations cantorDusts

kochCurve :: Int -> String
kochCurve = lsystemGeneration lsystem "F"
    where
        lsystem = lsystemFromRuleSet $ mkRuleSet [
                ('F', "F+F-F-F+F")
            ]

kochCurves :: [String]
kochCurves = lsystemGenerations lsystem "F"
    where
        lsystem = lsystemFromRuleSet $ mkRuleSet [
                ('F', "F+F-F-F+F")
            ]

kochCurveGrid :: Int -> G.Grid Char
kochCurveGrid numGenerations = mkLSystemGenerationsGrid $ take numGenerations kochCurves

sierpinskiTriangle :: Int -> String
sierpinskiTriangle = lsystemGeneration lsystem "A"
    where
        lsystem = lsystemFromRuleSet $ mkRuleSet [
                ('A', "B-A-B"),
                ('B', "A+B+A")
            ]

sierpinskiTriangles :: [String]
sierpinskiTriangles = lsystemGenerations lsystem "A"
    where
        lsystem = lsystemFromRuleSet $ mkRuleSet [
                ('A', "B-A-B"),
                ('B', "A+B+A")
            ]

sierpinskiTriangleGrid :: Int -> G.Grid Char
sierpinskiTriangleGrid numGenerations = mkLSystemGenerationsGrid $ take numGenerations sierpinskiTriangles

dragonCurve :: Int -> String
dragonCurve = lsystemGeneration lsystem "FX"
    where
        lsystem = lsystemFromRuleSet $ mkRuleSet [
                ('X', "X+YF+"),
                ('Y', "-FX-Y")
            ]

dragonCurves :: [String]
dragonCurves = lsystemGenerations lsystem "FX"
    where
        lsystem = lsystemFromRuleSet $ mkRuleSet [
                ('X', "X+YF+"),
                ('Y', "-FX-Y")
            ]

dragonCurveGrid :: Int -> G.Grid Char
dragonCurveGrid numGenerations = mkLSystemGenerationsGrid $ take numGenerations dragonCurves

fractalPlant :: Int -> String
fractalPlant = lsystemGeneration lsystem "X"
    where
        lsystem = lsystemFromRuleSet $ mkRuleSet [
                ('X', "F-[[X]+X]+F[+FX]-X"),
                ('F', "FF")
            ]

fractalPlants :: [String]
fractalPlants = lsystemGenerations lsystem "X"
    where
        lsystem = lsystemFromRuleSet $ mkRuleSet [
                ('X', "F-[[X]+X]+F[+FX]-X"),
                ('F', "FF")
            ]

fractalPlantGrid :: Int -> G.Grid Char
fractalPlantGrid numGenerations = mkLSystemGenerationsGrid $ take numGenerations fractalPlants
