module HelVM.HelPA.Assemblers.EAS.Translator where

import HelVM.HelPA.Assemblers.EAS.Instruction

import HelVM.HelPA.Common.Util
import HelVM.HelPA.Common.Value

import Data.List.Split
import Data.Char

import Numeric.Natural

import qualified Data.Map.Strict as Map

translate :: InstructionList -> InstructionList
translate il = replaceStrings $ replaceLabels addresses il
  where
    addresses = addressOfLabels il

----

type LabelAddresses = Map.Map String Natural

addressOfLabels :: InstructionList -> LabelAddresses
addressOfLabels il = Map.fromList $ zip (labelsToStrings2 il) [1..] >>= toList

labelsToStrings2 :: InstructionList -> [[String]]
labelsToStrings2 il = map labelsToStrings $ splitOn [R] il

labelsToStrings :: InstructionList -> [String]
labelsToStrings il = il >>= labelToStrings

labelToStrings :: Instruction -> [String]
labelToStrings (L s) = [s]
labelToStrings  _    = []

----

replaceLabels ::  LabelAddresses -> InstructionList -> InstructionList
replaceLabels addresses = map (replaceLabel addresses)

replaceLabel :: LabelAddresses -> Instruction -> Instruction
replaceLabel addresses (N (Variable l)) = N $ Literal $ findOrError l addresses
replaceLabel _          i               = i

----

replaceStrings :: InstructionList -> InstructionList
replaceStrings il = il >>= replaceString

replaceString :: Instruction -> InstructionList
replaceString (U s) = map charToInstruction $ reverse s 
replaceString  i    = [i]

charToInstruction :: Char -> Instruction
charToInstruction c = N $ Literal $ fromIntegral $ ord c
