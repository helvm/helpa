module HelVM.HelPA.Assemblers.Frontend.WSA.Reducer where

import qualified HelVM.HelPA.Assemblers.Backend.WSA.Instruction  as B
import           HelVM.HelPA.Assemblers.Frontend.WSA.Instruction

import           HelVM.HelPA.Assemblers.Backend.WSA.DSL

import           HelVM.HelPA.Assemblers.Common.DSL

import           Prelude                                         hiding (return, swap)

reduce :: Bool ->  InstructionList -> B.InstructionList
reduce b  = reduceInstructionList . addEndOfLineMaybe b

addEndOfLineMaybe :: Bool -> InstructionList -> InstructionList
addEndOfLineMaybe True  il = addEndOfLine =<< il
addEndOfLineMaybe False il = il

addEndOfLine :: Instruction -> InstructionList
addEndOfLine i = [i , EOL]

reduceInstructionList :: InstructionList -> B.InstructionList
reduceInstructionList = execDSL . reduceInstructionList_

reduceInstructionList_ :: MonadASM m => InstructionList -> m ()
reduceInstructionList_ = traverse_ reduceInstruction

reduceInstruction :: MonadASM m => Instruction -> m ()
reduceInstruction (Push v)        = push v
reduceInstruction (PushS s)       = reducePushS s
reduceInstruction Pop             = pop
reduceInstruction Dup             = dup
reduceInstruction Swap            = swap
reduceInstruction (Test v)        = reduceTest v
reduceInstruction (Add v)         = reduceAddOpt v
reduceInstruction (Sub v)         = reduceSubOpt v
reduceInstruction (Mul v)         = reduceMulOpt v
reduceInstruction (Div v)         = reduceDivOpt v
reduceInstruction (Mod v)         = reduceModOpt v
reduceInstruction (Load v)        = reduceLoadOpt v
reduceInstruction (Store v)       = reduceStoreOpt v
reduceInstruction (Mark   l)      = mark l
reduceInstruction (Call l)        = dsl $ B.Call l
reduceInstruction (Branch l)      = branch l
reduceInstruction (BranchZ l)     = branchZ l
reduceInstruction (BranchM l)     = branchM l
reduceInstruction (BranchNZ l)    = branchNZ l
reduceInstruction (BranchNM l)    = branchNM l
reduceInstruction (BranchP l)     = branchP l
reduceInstruction (BranchNP l)    = branchNP l
reduceInstruction Return          = return
reduceInstruction End             = end
reduceInstruction OutputChar      = dsl B.OutputChar
reduceInstruction OutputNum       = dsl B.OutputNum
reduceInstruction InputChar       = dsl B.InputChar
reduceInstruction InputNum        = dsl B.InputNum
reduceInstruction Noop            = dsl B.Noop
reduceInstruction DebugPrintStack = dsl B.DebugPrintStack
reduceInstruction DebugPrintHeap  = dsl B.DebugPrintHeap
reduceInstruction EOL             = dsl B.EOL
reduceInstruction i               = error $ show i
