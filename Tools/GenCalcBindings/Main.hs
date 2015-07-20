import System.Environment (getArgs)
import System.Console.GetOpt
import Data.Bits
import Data.Char

data Rate = ControlRate | AudioRate deriving (Show, Eq, Ord, Enum)
data CalcFuncBindingData = CalcFuncBindingData String String [Rate] deriving (Show)
data ArgDefine = ArgDefine String String Rate

argDefineName :: ArgDefine -> String
argDefineName (ArgDefine name _ _) = name

funcNameToCalcDefineName :: String -> String
funcNameToCalcDefineName funcName = (foldr (\s acc -> (map toUpper s) ++ "_" ++ acc) "" wordList) ++ "CALC"
    where
        wordList = filter ((>0) . length) $ (\(wrd, acc) -> wrd : acc) $ foldr splitCamelCase ("", [] :: [String]) funcName
        splitCamelCase char (wrd, acc) = if isUpper char then ("", (char : wrd) : acc) else (char : wrd, acc)

ugenNameToConstructorName :: String -> String
ugenNameToConstructorName ugenName = ugenName ++ "_constructor"

ugenNameToDeconstructorName :: String -> String
ugenNameToDeconstructorName ugenName = ugenName ++ "_deconstructor"

ugenNameToDataName :: String -> String
ugenNameToDataName ugenName = ugenName ++ "_data"

generateUGenDataStruct :: String -> String
generateUGenDataStruct ugenName = "typedef struct\n{\n    double accumulator; // example data\n} " ++ ugenNameToDataName ugenName ++ ";"

generateUGenConstructor :: String -> String -> String
generateUGenConstructor ugenName dataStructName = "void " ++ ugenNameToConstructorName ugenName ++ "(ugen* u)\n{\n" ++ constructorContents ++ "}"
    where
        constructorContents = lineOne ++ lineTwo ++ lineThree
        lineOne   = "    u->data = malloc(sizeof(" ++ dataStructName ++ "));\n"
        lineTwo   = "    " ++ dataStructName ++ " data = { 0 };\n"
        lineThree = "    *((" ++ dataStructName ++ "*) u->data) = data;\n"

generateUGenDeconstructor :: String -> String
generateUGenDeconstructor ugenName = "void " ++ ugenNameToDeconstructorName ugenName ++ "(ugen* u)\n{\n    free(u->data);\n}"

generateUGenStructors :: String -> String
generateUGenStructors ugenName = dataStructString ++ "\n\n" ++ constructorString ++ "\n\n" ++ deconstructorString ++ "\n\n"
    where
        dataStructName = ugenNameToDataName ugenName
        dataStructString = generateUGenDataStruct ugenName
        constructorString = generateUGenConstructor ugenName dataStructName
        deconstructorString = generateUGenDeconstructor ugenName

generateCalcDefine :: String -> String -> [String] -> String
generateCalcDefine funcName defineName args = inlineFunc ++ foldr (++) "" macroLinesWithEndings
    where
        lengthArgs = length args
        inlineFuncName = funcName ++ "_inline_calc"
        inlineFuncArgs = foldl (\a b -> a ++ (if null a then "" else ", ") ++ b) "" $ (dataName ++ "* data") : map (\s -> "double " ++ s) args
        inlineFunc = "static inline double " ++ inlineFuncName ++ "(" ++ inlineFuncArgs ++ ")\n{\n    double y = 0; // CALC CODE HERE\n    return y;\n}\n\n"
        calcDefineLine = "#define " ++ defineName ++ "(CONTROL_ARGS, AUDIO_ARGS)"
        dataName = ugenNameToDataName funcName
        ugenData = "(" ++ dataName ++ "*) u.data"
        dataDeclaration = dataName ++ "* data = " ++ ugenData ++ ";"
        inputs = map argIndexToInputString ([0 .. (max 0 (lengthArgs -1 ))] :: [Int])
        outputs = ["double* out = UGEN_OUTPUT_BUFFER(u, 0);"]
        argIndexToInputString index = "double* in" ++ show index ++ " = UGEN_INPUT_BUFFER(u, " ++ show index ++ ");"
        argLines = map (\arg -> "double " ++ arg ++ ";") args
        audioLoopFuncCall = "    UGEN_OUT(out, " ++ inlineFuncName ++ "(" ++ foldl (\a b -> a ++ (if null a then "" else ", ") ++ b) "" ("data" : args) ++ "));"
        macroLines = calcDefineLine : dataDeclaration : inputs ++ outputs ++ argLines ++ ["CONTROL_ARGS", "AUDIO_LOOP(", "    AUDIO_ARGS", audioLoopFuncCall, ");"]
        longestLength = foldr (\s acc -> max (length s) acc) 0 macroLines
        macroLinesWithEndings = map (\line -> line ++ replicate (max 0 (longestLength - length line)) ' ' ++ " \\\n") macroLines

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
generateCCode name args = generateUGenStructors name ++ (generateCalcDefine name calcDefineName args) ++ "\n\n" ++ argDefines ++ "\n\n" ++ cfuncs
    where
        numArgs = length args
        cfuncData = calcFuncBindingData name numArgs
        cfuncs = foldl (\acc s -> acc ++ s ++ "\n\n") "" $ map createCFunction [0..((2 ^ numArgs) - 1)]
        longestCFuncNameLength = foldl max (length "// Control Arguments  ") $ map (\(CalcFuncBindingData cname _ _) -> length cname) cfuncData
        calcDefineName = funcNameToCalcDefineName name
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
        createCFunction i = "// " ++ (show i) ++ "\n" ++ "void " ++ cname ++ "(ugen u)\n{\n    " ++ calcDefineName ++ "(" ++ fguts ++ "    )\n}"
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
        ugenConstructorBindingName = name ++ "Constructor"
        ugenDeconstructorBindingName = name ++ "Deconstructor"
        multiChannelString = "multiChannelExpandUGen " ++ (capitalizeFirst name) ++ " " ++ (last haskellNames) ++
                             " " ++ ugenConstructorBindingName ++ " " ++ ugenDeconstructorBindingName ++ " " ++ argsListString
        whereCode = "\n    where\n        cfuncs = " ++ haskellNamesListString
        numArgs = length args
        constructorBinding = "foreign import ccall \"&" ++ ugenNameToConstructorName name ++ "\" " ++ ugenConstructorBindingName ++ " :: CUGenFunc"
        deconstructorBinding = "foreign import ccall \"&" ++ ugenNameToDeconstructorName name ++ "\" " ++ ugenDeconstructorBindingName ++ " :: CUGenFunc"
        cbindingData = calcFuncBindingData name numArgs
        cbindings = constructorBinding ++ "\n" ++ deconstructorBinding ++ "\n" ++ (foldl (\acc b -> acc ++ b ++ "\n") "" $ calcFuncBindings name numArgs)
        haskellNames = map (\(CalcFuncBindingData _ hname _) -> hname) cbindingData
        haskellNamesListString = "[" ++ join ", " haskellNames ++ "]"
        argsListString = "[" ++ join ", " args ++ "]"

data Flag = Version deriving (Show, Eq, Enum)

options :: [OptDescr Flag]
options = [ Option ['V'] ["version"] (NoArg Version) "show version number" ]

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ show args
    let (flags, nonOpts, msgs) = getOpt RequireOrder options args
    print flags
    print nonOpts
    print msgs
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
