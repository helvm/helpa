module HelVM.HelPA.Assemblers.Frontend.EIR.Reducer where

import           HelVM.HelPA.Assemblers.Frontend.EIR.DSL
import           HelVM.HelPA.Assemblers.Frontend.EIR.Instruction

import           HelVM.HelPA.Assemblers.Common.DSL


import           HelVM.HelPA.Assembler.Value

--import           Control.Monad.RWS.Lazy

reduceInstruction :: (EIR a , MonadDSL a m) => Instruction -> m ()
reduceInstruction (Mov d s)      = mov s d
reduceInstruction (Add d s)      = add s d
reduceInstruction (Sub d s)      = sub s d
reduceInstruction (Load d s)     = load s d
reduceInstruction (Store s d)    = store s d
reduceInstruction (PutC s)       = putc s
reduceInstruction (GetC d)       = getc d
reduceInstruction (J c j d s)    = jComp c s d j
reduceInstruction (Jmp j)        = jmp j
reduceInstruction (L c d s)      = lComp c s d
reduceInstruction Dump           = dump
reduceInstruction (Mark j)       = mark j
reduceInstruction PText          = pText
reduceInstruction (PData s)      = pData s
reduceInstruction (PLong s)      = pLong s
reduceInstruction (PString s)    = pString s
reduceInstruction (PFile s s1)   = pFile s s1
reduceInstruction (PLoc s s1 s2) = pLoc s s1 s2
reduceInstruction i              = error $ show i

jComp :: (EIR a , MonadDSL a m) => Comp -> ImmediateORRegister -> Register -> Identifier -> m ()
jComp CEQ = jeq
jComp CNE = jne
jComp CLT = jlt
jComp CGT = jgt
jComp CLE = jle
jComp CGE = jge

lComp :: (EIR a , MonadDSL a m) => Comp -> ImmediateORRegister -> Register -> m ()
lComp CEQ = eq
lComp CNE = ne
lComp CLT = lt
lComp CGT = gt
lComp CLE = le
lComp CGE = ge
