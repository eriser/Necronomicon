module Necronomicon.Noise.Wolfram where

import Data.Bits
import qualified Data.Vector as V
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad

data WolframCell = White | Black deriving (Ord, Eq, Enum, Show)

-- Creates a rule vector to be used with wolframCA. The rule number is an integer from 0..255.
binaryWolframRuleVector :: Int -> V.Vector WolframCell
binaryWolframRuleVector ruleNumber = V.fromList $ foldr foldRule [] [max8BitIndex,  (max8BitIndex - 1) ..  min8BitIndex]
    where
        min8BitValue = 0
        max8BitValue = 255
        min8BitIndex = 0
        max8BitIndex = 7
        ruleNumber' = min max8BitValue (max min8BitValue ruleNumber)
        mask = 1
        foldRule i acc = toEnum (ruleNumber' `shiftR` (max8BitIndex - i) .&. mask) : acc

-- Elementary Cellular Automata, as defined by Wolfram. requires a rule vector created by binaryWolframRuleVector to be passed in
wolframCA :: V.Vector WolframCell -> V.Vector WolframCell -> V.Vector WolframCell
wolframCA cells ruleVector = V.fromList $ foldr binaryWolfram [] [0..maxCellIndex]
    where
        cellsLength = length cells
        maxCellIndex = max 0 (cellsLength - 1)
        ruleVectorLength = length ruleVector
        defaultBlankCell = White
        indexFunc left middle right = (fromEnum left `shiftL` 2) + (fromEnum middle `shiftL` 1) + fromEnum right
        binaryWolfram i acc = (ruleVector V.! rule) : acc
            where
                left = if leftIndex >= 0 then cells V.! leftIndex else defaultBlankCell where leftIndex = i - 1
                middle = cells V.! i
                right = if rightIndex < cellsLength then cells V.! rightIndex else defaultBlankCell where rightIndex = i + 1
                rule = min ruleVectorLength (max 0 (indexFunc left middle right))

-- Creates a rule map to be used with multiColoredWolframCA. Colors are Integers from 0 .. (numColors - 1). The rule can be any positive integer.
multiColoredWolframRuleMap :: Int -> Int -> M.Map Double Int
multiColoredWolframRuleMap numColors ruleNumber = foldl ruleNumberAndAverageToColor M.empty $ zip uniqueAveragesList [0 .. lastUniqueAveragesIndex]
    where
        uniqueAverages = foldl (\acc xs -> S.insert ((/numColorsDouble) $ sum xs) acc) S.empty $ replicateM numNeighbors [0.0 .. maxColorIndex]
        uniqueAveragesList = L.sort $ S.toList uniqueAverages
        lastUniqueAveragesIndex = max 0 $ (length uniqueAveragesList) - 1
        ruleNumberDouble = fromIntegral (max 0 ruleNumber) :: Double
        numColorsDouble = fromIntegral numColors :: Double
        numNeighbors = 3
        maxColorIndex = fromIntegral . max 0 $ numColors - 1
        ruleNumberAndAverageToColor acc (average, i) = M.insert average color acc
            where
                color = mod (floor (ruleNumberDouble / (fromIntegral (numColors ^ i)))) numColors

-- Multicolored Wolframa Cellular Automata. Requires a rule map created with multiColoredWolframRuleMap to be passed in.
multiColoredWolframCA :: V.Vector Int -> M.Map Double Int -> V.Vector Int
multiColoredWolframCA cells ruleMap = V.fromList $ foldr multiColoredWolfram [] [0..maxCellIndex]
    where
        numNeighbors = 3 :: Double
        defaultBlankCell = 0 :: Int
        cellsLength = length cells
        maxCellIndex = max 0 (cellsLength - 1)
        multiColoredWolfram i acc = color : acc
            where
                left = if leftIndex >= 0 then cells V.! leftIndex else defaultBlankCell where leftIndex = i - 1
                middle = cells V.! i
                right = if rightIndex < cellsLength then cells V.! rightIndex else defaultBlankCell where rightIndex = i + 1
                rule = fromIntegral (left + middle + right) / numNeighbors
                color = case (M.lookup rule ruleMap) of
                    Nothing -> defaultBlankCell
                    Just c -> c
