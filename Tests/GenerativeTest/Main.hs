import Necronomicon

main :: IO ()
main = mapM_ (\(name, gen) -> putStrLn (formatName name) >> (mapM_ (print . gen) iterations) >> (putStrLn "\n")) lsystems
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
