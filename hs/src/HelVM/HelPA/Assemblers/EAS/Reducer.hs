{-# LANGUAGE TupleSections #-}
module HelVM.HelPA.Assemblers.EAS.Reducer (
  reduce
) where

import           HelVM.HelPA.Assemblers.EAS.Instruction

import           HelVM.HelPA.Assembler.Value

import           HelVM.Common.Containers.Util
import           HelVM.Common.Safe

import           Data.List.Split

import qualified Data.ListLike                          as LL

reduce :: MonadSafeError m => InstructionList -> m InstructionList
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

replaceLabels ::  MonadSafeError m => LabelAddresses -> InstructionList -> m InstructionList
replaceLabels addresses il = sequenceA $ replaceLabel addresses <$> il

replaceLabel :: MonadSafeError m => LabelAddresses -> Instruction -> m Instruction
replaceLabel addresses (N (Variable l)) = N . Literal <$> indexSafeByKey l addresses
replaceLabel _          i               = pure i

----

replaceStrings :: InstructionList -> InstructionList
replaceStrings il = replaceString =<< il

replaceString :: Instruction -> InstructionList
replaceString (U s) = charToInstruction <$> toList (LL.reverse s)
replaceString  i    = [i]

charToInstruction :: Char -> Instruction
charToInstruction = N . Literal . fromIntegral . ord

----

setDefault :: ([k] , v) -> [(k , v)]
setDefault (keys , value) = (, value) <$> keys
