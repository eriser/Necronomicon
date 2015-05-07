module Necronomicon.Noise.Wolfram where

import Data.Bits
import qualified Data.Vector as V

data WolframCell = White | Black deriving (Ord, Eq, Enum, Show)

binaryWolframRuleVector :: Int -> V.Vector WolframCell
binaryWolframRuleVector ruleSet = V.fromList $ foldr foldRule [] [max8BitIndex,  (max8BitIndex - 1) ..  min8BitIndex]
    where
        min8BitValue = 0
        max8BitValue = 255
        min8BitIndex = 0
        max8BitIndex = 7
        ruleSet' = min max8BitValue (max min8BitValue ruleSet)
        mask = 1
        foldRule i acc = toEnum (ruleSet' `shiftR` (max8BitIndex - i) .&. mask) : acc

wolframCA :: V.Vector WolframCell -> V.Vector WolframCell -> V.Vector WolframCell
wolframCA cells ruleList = V.fromList $ foldr binaryWolfram [] [0..maxCellIndex]
    where
        cellsLength = length cells
        maxCellIndex = max 0 (cellsLength - 1)
        ruleListLength = length ruleList
        binaryWolfram i acc = (ruleList V.! rule) : acc
            where
                left = if leftIndex >= 0 then cells V.! leftIndex else White where leftIndex = i - 1
                middle = cells V.! i
                right = if rightIndex < cellsLength then cells V.! rightIndex else White where rightIndex = i + 1
                rule = min ruleListLength (max 0 ((fromEnum left `shiftL` 2) + (fromEnum middle `shiftL` 1) + fromEnum right))
