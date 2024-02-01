module HelVM.HelPA.Assemblers.Frontend.WSA.Reducer where

import qualified HelVM.HelPA.Assemblers.Backend.WSA.Instruction  as B
import           HelVM.HelPA.Assemblers.Frontend.WSA.Instruction

import           HelVM.HelPA.Assembler.Value

import           HelVM.HelPA.Assemblers.Backend.WSA.DSL

import           Prelude                                         hiding (return, swap)

reduce :: Bool ->  InstructionList -> B.InstructionList
reduce b  = reduceInstructionList . addEndOfLineMaybe b

addEndOfLineMaybe :: Bool -> InstructionList -> InstructionList
addEndOfLineMaybe True  il = addEndOfLine =<< il
addEndOfLineMaybe False il = il

addEndOfLine :: Instruction -> InstructionList
addEndOfLine i = [i , EOL]

reduceInstructionList :: InstructionList -> B.InstructionList
reduceInstructionList il = uncurry reduceInstructionWithExec =<< zipIndex il

reduceInstructionWithExec :: Natural -> Instruction -> B.InstructionList
reduceInstructionWithExec n = execDSL n . reduceInstruction

reduceInstruction :: DSL m => Instruction -> m ()
reduceInstruction (Push v)            = push v
reduceInstruction (PushS (Literal s)) = reducePushS s
reduceInstruction Pop                 = pop
reduceInstruction Dup                 = dup
reduceInstruction Swap                = swap
reduceInstruction (Test v)            = reduceTest $ Literal v
reduceInstruction (Add v)             = reduceAddOpt v
reduceInstruction (Sub v)             = reduceSubOpt v
reduceInstruction (Mul v)             = reduceMulOpt v
reduceInstruction (Div v)             = reduceDivOpt v
reduceInstruction (Mod v)             = reduceModOpt v
reduceInstruction (Load v)            = reduceLoadOpt v
reduceInstruction (Store v)           = reduceStoreOpt v
reduceInstruction (Mark   l)          = mark l
reduceInstruction (Call l)            = dsl $ B.Call l
reduceInstruction (Branch l)          = branch l
reduceInstruction (BranchZ l)         = branchZ l
reduceInstruction (BranchM l)         = branchM l
reduceInstruction (BranchNZ l)        = reduceBranchNZ l
reduceInstruction (BranchNM l)        = reduceBranchNM l
reduceInstruction (BranchP l)         = reduceBranchP l
reduceInstruction (BranchNP l)        = reduceBranchNP l
reduceInstruction Return              = return
reduceInstruction End                 = end
reduceInstruction OutputChar          = dsl B.OutputChar
reduceInstruction OutputNum           = dsl B.OutputNum
reduceInstruction InputChar           = dsl B.InputChar
reduceInstruction InputNum            = dsl B.InputNum
reduceInstruction Noop                = dsl B.Noop
reduceInstruction DebugPrintStack     = dsl B.DebugPrintStack
reduceInstruction DebugPrintHeap      = dsl B.DebugPrintHeap
reduceInstruction EOL                 = dsl B.EOL
reduceInstruction i                   = error $ show i

zipIndex :: [a] -> [(Natural, a)]
zipIndex = zip [0 ..]
