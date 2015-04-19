import System.Environment (getArgs)
import Data.Bits
import Data.Char

data Rate = ControlRate | AudioRate deriving (Show, Eq, Ord, Enum)
data CalcFuncBindingData = CalcFuncBindingData String String [Rate] deriving (Show)
data ArgDefine = ArgDefine String String Rate

argDefineName :: ArgDefine -> String
argDefineName (ArgDefine name _ _) = name

-- Creates a little endian bit list representation of an Int
toBitList :: Int -> [Int]
toBitList i = map (\b -> if (i .&. b) > 0 then 1 else 0) (takeWhile (<=i) $ map (2^) ([0..] :: [Int]))

-- Creates a little endian bit list (with a fixed length) representation of an Int
toBitListFixedLength :: Int -> Int -> [Int]
toBitListFixedLength i l = take l (toBitList i ++ cycle [0])

-- Creates a list of all the permutations of calc func binding data for control and audio rate ugens
-- The list generation is exponential, so functions with 2 arguments will produce lists of length 4, and 3 argument functions will produce lists of length 8
calcFuncBindingData :: String -> Int -> [CalcFuncBindingData]
calcFuncBindingData name numArgs = map makeBindingIteration [0..((2 ^ numArgs) - 1)]
    where
        makeBindingIteration n = CalcFuncBindingData (name ++ "_" ++ controlChars ++ "_calc") (name ++ (map toUpper controlChars) ++ "Calc") ((map toEnum bitList) :: [Rate])
            where
                controlChars = map (\b -> if b > 0 then 'a' else 'k') bitList
                bitList = toBitListFixedLength n numArgs

calcFuncBindings :: String -> Int -> [String]
calcFuncBindings name numArgs = map (funcDataToBinding) cfuncData
    where
        cfuncData = calcFuncBindingData name numArgs
        funcDataToBinding (CalcFuncBindingData fname fbind _) = "foreign import ccall \"&" ++ fname ++ "\" " ++ fbind ++ " :: CUGenFunc"

generateCCode :: String -> [String] -> String
generateCCode name args = argDefines ++ "\n\n" ++ cfuncs
    where
        numArgs = length args
        cfuncData = calcFuncBindingData name numArgs
        cfuncs = foldl (\acc s -> acc ++ s ++ "\n\n") "" $ map createCFunction [0..((2 ^ numArgs) - 1)]
        longestCFuncNameLength = foldl max (length "// Control Arguments  ") $ map (\(CalcFuncBindingData cname _ _) -> length cname) cfuncData
        argDefines = foldl (\acc (ArgDefine _ ks _, ArgDefine _ as _) -> acc ++ ks ++ "\n" ++ as ++ "\n") "" argDefinesList
        argDefinesList :: [(ArgDefine, ArgDefine)]
        argDefinesList = map argToDefines $ zip args [0..]
        argToControlDefine arg i = ArgDefine defineName ("#define " ++ defineName ++ " " ++ arg ++ " = *in" ++ (show i) ++ ";") ControlRate
            where
                defineName = (map toUpper name) ++ "_" ++ (map toUpper arg) ++ "K"
        argToAudioDefine arg i = ArgDefine defineName ("#define " ++ defineName ++ " " ++ arg ++ " = UGEN_IN(in" ++ (show i) ++ ");") AudioRate
            where
                defineName = (map toUpper name) ++ "_" ++ (map toUpper arg) ++ "A"
        argToDefines :: (String, Int) -> (ArgDefine, ArgDefine)
        argToDefines (arg, i) = (argToControlDefine arg i, argToAudioDefine arg i)
        createCFunction :: Int -> String
        createCFunction i = "// " ++ (show i) ++ "\n" ++ "void " ++ cname ++ "(ugen u)\n{\n    " ++ (map toUpper name) ++ "_CALC(" ++ fguts ++ "    )\n}"
            where
                (CalcFuncBindingData cname _ rates) = cfuncData !! i
                fguts = argPrefix ++ "// Control Arguments" ++ controlArgs' ++ "," ++ argPrefix ++ "// Audio Arguments" ++ audioArgs' ++ "\n"
                (controlArgs, audioArgs) = foldl foldRate ("", "") $ zip rates [0..]
                controlArgs' = if null controlArgs then argPrefix ++ "/* no control args */" else controlArgs
                audioArgs' = if null audioArgs then argPrefix ++ "/* no audio args */" else audioArgs
                argPrefix = "\n        "
                foldRate (cs, as) (ControlRate, argIndex) = (cs ++ argPrefix ++ defineString ++ pad ++ "/* " ++ (show argIndex) ++ " */", as)
                    where
                        defineString = argDefineName $ fst (argDefinesList !! argIndex)
                        pad = replicate (longestCFuncNameLength - length defineString) ' '
                foldRate (cs, as) (AudioRate, argIndex) = (cs, as ++ argPrefix ++ defineString ++ pad ++ "/* " ++ (show argIndex) ++ " */")
                    where
                        defineString = argDefineName $ snd (argDefinesList !! argIndex)
                        pad = replicate (longestCFuncNameLength - length defineString) ' '

generateHaskellCode :: String -> [String] -> String
generateHaskellCode name args = cbindings ++ "\n" ++ typeSignature ++ "\n" ++ funcCode ++ whereCode
    where
        join js = foldl (\acc s -> if (not $ null acc) && (not $ null s) then acc ++ js ++ s else acc ++ s) ""
        typeSignature = name ++ " :: " ++ (join " -> " $ replicate (length args + 1) "UGen")
        capitalizeFirst [] = ""
        capitalizeFirst (c:cs) = toUpper c : cs
        funcCode = (join " " (name : args)) ++ " = " ++ optimizeString ++ multiChannelString
        optimizeString = "optimizeUGenCalcFunc cfuncs $ "
        multiChannelString = "multiChannelExpandUGen " ++ (capitalizeFirst name) ++ " " ++ (last haskellNames) ++ " " ++ name ++ "Constructor " ++ name ++ "Deconstructor " ++ argsListString
        whereCode = "\n    where\n        cfuncs = " ++ haskellNamesListString
        numArgs = length args
        cbindingData = calcFuncBindingData name numArgs
        cbindings = foldl (\acc b -> acc ++ b ++ "\n") "" $ calcFuncBindings name numArgs
        haskellNames = map (\(CalcFuncBindingData _ hname _) -> hname) cbindingData
        haskellNamesListString = "[" ++ join ", " haskellNames ++ "]"
        argsListString = "[" ++ join ", " args ++ "]"

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ show args
    let ugenName = head args
    let ugenArgs = tail args
    putStrLn "////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////"
    putStrLn "// C Code "
    putStrLn "////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////\n"
    putStrLn $ generateCCode ugenName ugenArgs
    putStrLn "------------------------------------------------------------------------------------------------------------------------"
    putStrLn "-- Haskell Code"
    putStrLn "------------------------------------------------------------------------------------------------------------------------\n"
    putStrLn $ generateHaskellCode ugenName ugenArgs
