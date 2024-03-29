module HelVM.HelPA.Assemblers.Backend.EAS.Reducer (
  reduce
) where

import           HelVM.HelPA.Assemblers.Backend.EAS.Instruction

import           HelVM.HelPA.Assembler.Extra
import           HelVM.HelPA.Assembler.Value

import           HelVM.HelIO.Containers.Extra
import           HelVM.HelIO.Control.Safe

import           Data.List.Split

import qualified Data.ListLike                                  as LL

reduce :: MonadSafe m => InstructionList -> m InstructionList
reduce il = replaceStrings <$> replaceLabels addresses il where addresses = addressOfLabels il

----

addressOfLabels :: InstructionList -> LabelAddresses
addressOfLabels il = flippedToMapFromLists [1..] $ (labelToIdentifiers =<<) <$> splitOn [R] il

----

replaceLabels ::  MonadSafe m => LabelAddresses -> InstructionList -> m InstructionList
replaceLabels addresses = traverse (replaceLabel addresses)

replaceLabel :: MonadSafe m => LabelAddresses -> Instruction -> m Instruction
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
