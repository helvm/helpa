module HelVM.HelPA.Assemblers.EAS.Reducer (
  reduce
) where

import HelVM.HelPA.Assemblers.EAS.Instruction

import HelVM.HelPA.Common.OrError
import HelVM.HelPA.Common.Util
import HelVM.HelPA.Common.Value

import Data.List.Split

reduce :: InstructionList -> InstructionList
reduce il = replaceStrings $ replaceLabels addresses il where addresses = addressOfLabels il

----

type LabelAddresses = Map String Natural

addressOfLabels :: InstructionList -> LabelAddresses
addressOfLabels il = fromList $ setDefault =<< zip (labelsToStrings2 il) [1..]

labelsToStrings2 :: InstructionList -> [[String]]
labelsToStrings2 il = labelsToStrings <$> splitOn [R] il

labelsToStrings :: InstructionList -> [String]
labelsToStrings il = labelToStrings =<< il

labelToStrings :: Instruction -> [String]
labelToStrings (L s) = [s]
labelToStrings  _    = []

----

replaceLabels ::  LabelAddresses -> InstructionList -> InstructionList
replaceLabels addresses il = replaceLabel addresses <$> il

replaceLabel :: LabelAddresses -> Instruction -> Instruction
replaceLabel addresses (N (Variable l)) = N $ Literal $ findOrError l addresses
replaceLabel _          i               = i

----

replaceStrings :: InstructionList -> InstructionList
replaceStrings il = replaceString =<< il

replaceString :: Instruction -> InstructionList
replaceString (U s) = charToInstruction <$> reverse s
replaceString  i    = [i]

charToInstruction :: Char -> Instruction
charToInstruction = N . Literal . fromIntegral . ord
