import Necronomicon
import Control.Monad
import qualified Data.Vector as V

main :: IO ()
main = testLSystem >> testWolfram

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
    mapM_ (\n -> printWolframRule n >> putStrLn "\n") ([0..255] :: [Int])
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
