import Necronomicon
import Control.Monad
import qualified Data.Vector as V
import qualified Necronomicon.Util.Grid as G

main :: IO ()
main = do
    testLSystem
    testLSystemGenerations
    testLSystemGenerationGrids
    testWolfram
    testMultiColoredWolfram
    testInfiniteWolframGrid
    testWolframGrid
    testMultiColoredWolframGrid
    mapM_ testMultiColoredWolframGrid8Colors ([3150000..3160000] :: [Int])

testLSystem :: IO ()
testLSystem = do
    print "--------------------------------------------------------------------------------------------------------------------"
    print "-- L-System Generation"
    print "--------------------------------------------------------------------------------------------------------------------"
    putStrLn "\n"
    mapM_ (\(name, gen) -> putStrLn (formatName name) >> (mapM_ (print . gen) iterations) >> (putStrLn "\n")) lsystems
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

testLSystemGenerations :: IO ()
testLSystemGenerations = do
    print "--------------------------------------------------------------------------------------------------------------------"
    print "-- L-System Generations"
    print "--------------------------------------------------------------------------------------------------------------------"
    putStrLn "\n"
    mapM_ printLSystems lsystems
    where
        numGenerations = 6
        formatName n = divider ++ "\n-- " ++ n ++ "\n" ++ divider ++ "\n"
        divider = "--------------------------------------------------"
        lsystems :: [(String, [String])]
        lsystems = [
                ("algaeSystems", algaeSystems),
                ("pythagorasTrees", pythagorasTrees),
                ("cantorDusts", cantorDusts),
                ("kochCurves", kochCurves),
                ("sierpinskiTriangles", sierpinskiTriangles),
                ("dragonCurves", dragonCurves),
                ("fractalPlants", fractalPlants)
            ]
        printLSystems (name, lsystem) = putStrLn (formatName name) >> mapM_ print (take numGenerations lsystem) >> putStrLn "\n"

testLSystemGenerationGrids :: IO ()
testLSystemGenerationGrids = do
    print "--------------------------------------------------------------------------------------------------------------------"
    print "-- L-System Generation Grids"
    print "--------------------------------------------------------------------------------------------------------------------"
    putStrLn "\n"
    mapM_ printLSystems lsystems
    where
        numGenerations = 4
        formatName n = divider ++ "\n-- " ++ n ++ "\n" ++ divider ++ "\n"
        divider = "--------------------------------------------------"
        lsystems :: [(String, (Int -> G.Grid Char))]
        lsystems = [
                ("algaeSystemGrid", algaeSystemGrid),
                ("pythagorasTreeGrid", pythagorasTreeGrid),
                ("cantorDustGrid", cantorDustGrid),
                ("kochCurveGrid", kochCurveGrid),
                ("sierpinskiTriangleGrid", sierpinskiTriangleGrid),
                ("dragonCurveGrid", dragonCurveGrid),
                ("fractalPlantGrid", fractalPlantGrid)
            ]
        printLSystems (name, lsystem) = putStrLn (formatName name) >> print (lsystem numGenerations) >> putStrLn "\n"

wolframCellToAscii :: WolframCell -> Char
wolframCellToAscii cell = case cell of
    White -> ' '
    Black -> '#'

colorToAscii :: Int -> Char
colorToAscii n = toEnum (n + 93)

numRows :: Int
numRows = 50

lastRowIndex :: Int
lastRowIndex = numRows - 1

testWolfram :: IO ()
testWolfram = do
    print "--------------------------------------------------------------------------------------------------------------------"
    print "-- Wolfram"
    print "--------------------------------------------------------------------------------------------------------------------"
    mapM_ (\n -> printWolframRule n >> putStrLn "\n") ([18, 30, 73, 103, 105, 110, 150, 169, 193, 197] :: [Int])
    where
        startingRow = V.fromList (replicate 80 White ++ [Black] ++ replicate 80 White)
        startingRowInts = V.map wolframCellToAscii startingRow
        printWolframRule rule = do
            print $ "Rule " ++ show rule
            print startingRowInts
            foldM_ printAndfoldWolfram startingRow ([0..lastRowIndex] :: [Int])
            where
                ruleVector = binaryWolframRuleVector rule
                printAndfoldWolfram cells _ = do
                    let cells' = wolframCA cells ruleVector
                    print $ V.map wolframCellToAscii cells'
                    return cells'

testWolframGrid :: IO ()
testWolframGrid = do
    putStrLn "\n"
    print "--------------------------------------------------------------------------------------------------------------------"
    print "-- Wolfram Grid"
    print "--------------------------------------------------------------------------------------------------------------------"
    putStrLn "\n"
    let seedCells = V.fromList (replicate 80 White ++ [Black] ++ replicate 80 White)
        ruleVector = binaryWolframRuleVector 105
        wolframCAGrid = G.map wolframCellToAscii $ mkBinaryWolframGrid seedCells ruleVector numRows
    print wolframCAGrid

testMultiColoredWolframGrid :: IO ()
testMultiColoredWolframGrid = do
    putStrLn "\n"
    print "--------------------------------------------------------------------------------------------------------------------"
    print "-- Multi-colored Wolfram Grid"
    print "--------------------------------------------------------------------------------------------------------------------"
    putStrLn "\n"
    let seedCells = V.fromList (replicate 80 0 ++ [1] ++ replicate 80 0)
        numColors = 3
        rule = 573377
        ruleMap = multiColoredWolframRuleMap numColors rule
        wolframCAGrid = G.map colorToAscii $ mkMultiColoredWolframGrid seedCells ruleMap numRows
    print wolframCAGrid

testMultiColoredWolframGrid8Colors :: Int -> IO ()
testMultiColoredWolframGrid8Colors rule = do
    putStrLn "\n"
    print "--------------------------------------------------------------------------------------------------------------------"
    print "-- Multi-colored Wolfram Grid: 8 Colors"
    print "--------------------------------------------------------------------------------------------------------------------"
    putStrLn "\n"
    let seedCells = V.fromList . take 161 $ map (flip mod numColors) [0,5..]
        numColors = 8
        ruleMap = multiColoredWolframRuleMap numColors rule
        wolframCAGrid = G.map colorToAscii $ mkMultiColoredWolframGrid seedCells ruleMap numRows
    print wolframCAGrid

testInfiniteWolframGrid :: IO ()
testInfiniteWolframGrid = do
    putStrLn "\n"
    print "--------------------------------------------------------------------------------------------------------------------"
    print "-- Infinite Wolfram Grid"
    print "--------------------------------------------------------------------------------------------------------------------"
    putStrLn "\n"
    let seedCells = (replicate 80 White ++ [Black] ++ replicate 80 White)
        seedCellsVector = V.fromList seedCells
        seedCellsIndexes = [0..length seedCells - 1] :: [Int]
        ruleVector = binaryWolframRuleVector 169
        infWolframCA = mkInfiniteWolframGrid seedCellsVector ruleVector
        rowNums = [0..lastRowIndex] :: [Int]
    mapM_ (\y -> (print $ map (\x -> wolframCellToAscii $ lookupInfiniteGrid infWolframCA x y) seedCellsIndexes)) rowNums

testMultiColoredWolfram :: IO ()
testMultiColoredWolfram = do
    putStrLn "\n"
    print "--------------------------------------------------------------------------------------------------------------------"
    print "-- Multi-Colored Wolfram"
    print "--------------------------------------------------------------------------------------------------------------------"
    putStrLn "\n"
    mapM_ (\n -> printWolframRule n >> putStrLn "\n") ([77245, 210819, 325311, 497049, 573377, 706951, 974099] :: [Int])
    where
        numColors = 3
        startingRow = V.fromList (replicate 80 0 ++ [1] ++ replicate 80 0)
        startingRowAscii = V.map colorToAscii startingRow
        printWolframRule rule = do
            print $ "Number of Colors: " ++ show numColors ++ " Rule: " ++ show rule
            print startingRowAscii
            foldM_ printAndfoldWolfram startingRow ([0..lastRowIndex] :: [Int])
            where
                ruleMap = multiColoredWolframRuleMap numColors rule
                printAndfoldWolfram cells _ = do
                    let cells' = multiColoredWolframCA cells ruleMap
                    print $ V.map colorToAscii cells'
                    return cells'
