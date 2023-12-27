module HelVM.HelPA.Assemblers.WSA.Reducer where

import           HelVM.HelPA.Assemblers.WSA.Instruction

import           HelVM.HelPA.Assembler.Value

import           HelVM.HelPA.Assemblers.WSA.DSL

import qualified Data.ListLike                          as LL

reduce :: Bool ->  InstructionList -> InstructionList
reduce b  = reduceInstructionList . addEndOfLineMaybe b

addEndOfLineMaybe :: Bool -> InstructionList -> InstructionList
addEndOfLineMaybe True  il = addEndOfLine =<< il
addEndOfLineMaybe False il = il

addEndOfLine :: Instruction -> InstructionList
addEndOfLine i = [i , EOL]

reduceInstructionList :: InstructionList -> InstructionList
reduceInstructionList il = uncurry reduceInstruction =<< zipIndex il

reduceInstruction :: Natural -> Instruction -> InstructionList
reduceInstruction _ (PushS (Literal s)) = reducePushS s
reduceInstruction n (Test v)            = execDSL n $ reduceTest (Literal v)
reduceInstruction n (Load  (Just v))    = execDSL n $ reduceLoad v
reduceInstruction n (Store (Just v))    = execDSL n $ reduceStore v
reduceInstruction n (Add (Just v))      = execDSL n $ reduceAdd v
reduceInstruction n (Sub (Just v))      = execDSL n $ reduceSub v
reduceInstruction n (Mul (Just v))      = execDSL n $ reduceMul v
reduceInstruction n (Div (Just v))      = execDSL n $ reduceDiv v
reduceInstruction n (Mod (Just v))      = execDSL n $ reduceMod v
reduceInstruction n (BranchNZ l)        = execDSL n $ reduceBranchNZ l
reduceInstruction n (BranchNM l)        = execDSL n $ reduceBranchNM l
reduceInstruction n (BranchP l)         = execDSL n $ reduceBranchP l
reduceInstruction n (BranchNP l)        = execDSL n $ reduceBranchNP l

reduceInstruction _ i                    = [i]

reducePushS :: LL.ListLike t Char => t -> InstructionList
reducePushS s = reducePushS' $ LL.uncons s where
  reducePushS'  Nothing        = [pushLiteral 0]
  reducePushS' (Just (x , xs)) = reducePushS xs <> [pushLiteral $ fromIntegral $ ord x]


zipIndex :: [a] -> [(Natural, a)]
zipIndex = zip [0 ..]
