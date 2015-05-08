import Necronomicon
import Control.Monad
import qualified Data.Vector as V

main :: IO ()
main = testLSystem >> testWolfram >> testMultiColoredWolfram

testLSystem :: IO ()
testLSystem = mapM_ (\(name, gen) -> putStrLn (formatName name) >> (mapM_ (print . gen) iterations) >> (putStrLn "\n")) lsystems
    where
        formatName n = divider ++ "\n-- " ++ n ++ "\n" ++ divider ++ "\n"
        divider = "--------------------------------------------------"
        iterations = [0..3] :: [Int]
        lsystems :: [(String, (Int -> String))]
        lsystems = [
                ("algaeSystem", algaeSystem),
                ("pythagorasTree", pythagorasTree),
                ("cantorDust", cantorDust),
                ("kochCurve", kochCurve),
                ("sierpinskiTriangle", sierpinskiTriangle),
                ("dragonCurve", dragonCurve),
                ("fractalPlant", fractalPlant)
            ]

testWolfram :: IO ()
testWolfram = do
    print "--------------------------------------------------------------------------------------------------------------------"
    print "-- Wolfram"
    print "--------------------------------------------------------------------------------------------------------------------"
    mapM_ (\n -> printWolframRule n >> putStrLn "\n") ([18, 30, 73, 103, 105, 110, 150, 169, 193, 197] :: [Int])
    where
        wolframCellToAscii cell = case cell of
            White -> ' '
            Black -> '#'
        startingRow = V.fromList (replicate 80 White ++ [Black] ++ replicate 80 White)
        startingRowInts = V.map wolframCellToAscii startingRow
        printWolframRule rule = do
            print $ "Rule " ++ show rule
            print startingRowInts
            foldM_ printAndfoldWolfram startingRow ([0..50] :: [Int])
            where
                ruleVector = binaryWolframRuleVector rule
                printAndfoldWolfram cells _ = do
                    let cells' = wolframCA cells ruleVector
                    print $ V.map wolframCellToAscii cells'
                    return cells'

testMultiColoredWolfram :: IO ()
testMultiColoredWolfram = do
    print "--------------------------------------------------------------------------------------------------------------------"
    print "-- Multi-Colored Wolfram"
    print "--------------------------------------------------------------------------------------------------------------------"
    mapM_ (\n -> printWolframRule n >> putStrLn "\n") ([77245, 210819, 325311, 497049, 573377, 706951, 974099] :: [Int])
    where
        numColors = 3
        toAscii :: Int -> Char
        toAscii n = toEnum (n + 93)
        startingRow = V.fromList (replicate 80 0 ++ [1] ++ replicate 80 0)
        startingRowAscii = V.map toAscii startingRow
        printWolframRule rule = do
            print $ "Number of Colors: " ++ show numColors ++ " Rule: " ++ show rule
            print startingRowAscii
            foldM_ printAndfoldWolfram startingRow ([0..50] :: [Int])
            where
                ruleMap = multiColoredWolframRuleMap numColors rule
                printAndfoldWolfram cells _ = do
                    let cells' = multiColoredWolframCA cells ruleMap
                    print $ V.map toAscii cells'
                    return cells'
