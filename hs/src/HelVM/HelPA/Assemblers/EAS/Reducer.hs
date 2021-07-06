{-# LANGUAGE TupleSections #-}
module HelVM.HelPA.Assemblers.EAS.Reducer (
  reduce
) where

import HelVM.HelPA.Assemblers.EAS.Instruction

import HelVM.HelPA.Assembler.Value

import HelVM.Common.Containers.Util
import HelVM.Common.Safe

import Data.List.Split

reduce :: InstructionList -> Safe InstructionList
reduce il = replaceStrings <$> replaceLabels addresses il where addresses = addressOfLabels il

----

type LabelAddresses = Map Identifier Natural

addressOfLabels :: InstructionList -> LabelAddresses
addressOfLabels il = fromList $ setDefault =<< zip (labelsToIdentifiers2 il) [1..]

labelsToIdentifiers2 :: InstructionList -> [[Identifier]]
labelsToIdentifiers2 il = labelsToIdentifiers <$> splitOn [R] il

labelsToIdentifiers :: InstructionList -> [Identifier]
labelsToIdentifiers il = labelToIdentifiers =<< il

labelToIdentifiers :: Instruction -> [Identifier]
labelToIdentifiers (L s) = [s]
labelToIdentifiers  _    = []

----

replaceLabels ::  LabelAddresses -> InstructionList -> Safe InstructionList
replaceLabels addresses il = sequenceA $ replaceLabel addresses <$> il

replaceLabel :: LabelAddresses -> Instruction -> Safe Instruction
replaceLabel addresses (N (Variable l)) = N . Literal <$> indexSafeByKey l addresses
replaceLabel _          i               = safe i

----

replaceStrings :: InstructionList -> InstructionList
replaceStrings il = replaceString =<< il

replaceString :: Instruction -> InstructionList
replaceString (U s) = charToInstruction <$> reverse s
replaceString  i    = [i]

charToInstruction :: Char -> Instruction
charToInstruction = N . Literal . fromIntegral . ord

----

setDefault :: ([k] , v) -> [(k , v)]
setDefault (keys , value) = (, value) <$> keys
