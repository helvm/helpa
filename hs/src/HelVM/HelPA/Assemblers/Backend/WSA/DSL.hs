module HelVM.HelPA.Assemblers.Backend.WSA.DSL where

import           HelVM.HelPA.Assemblers.Backend.WSA.Instruction

import           HelVM.HelPA.Assemblers.Common.Config
import           HelVM.HelPA.Assemblers.Common.Environment

import           HelVM.HelPA.Assembler.Value

import           HelVM.HelIO.Collections.SList

import           Control.Monad.Writer.Lazy

import           Control.Monad.RWS.Lazy

import           HelVM.HelPA.Assemblers.Common.DSL

import qualified Data.ListLike                                  as LL

import           Prelude                                        hiding (div, mod, swap)

execDSL :: DSL Instruction -> InstructionList
execDSL w = snd $ evalRWS w makeConfig makeEnvironment

reducePushS :: MonadWriter InstructionList m => SString -> m ()
reducePushS = tell . pushLiteralS

pushLiteralS :: SString -> InstructionList
pushLiteralS s = pushLiteral 0 : toList (LL.reverse $ pushLiteralC <$> s)

pushLiteralC :: Char -> Instruction
pushLiteralC = pushLiteral . fromIntegral . ord

push :: MonadASM m => Integer -> m ()
push = dsl . Push

pop :: MonadASM0 m
pop = dsl Pop

dup :: MonadASM0 m
dup = dsl Dup

swap :: MonadASM0 m
swap = dsl Swap

reduceTest :: MonadASM m => Integer -> m ()
reduceTest v = do
  dup
  reduceSub v

reduceAddOpt :: MonadASM m => Maybe Integer -> m ()
reduceAddOpt Nothing  = add
reduceAddOpt (Just v) = reduceAdd v

reduceAdd :: MonadASM m => Integer -> m ()
reduceAdd v = do
  push v
  add

add :: MonadASM0 m
add = dsl Add

reduceSubOpt :: MonadASM m => Maybe Integer -> m ()
reduceSubOpt Nothing  = sub
reduceSubOpt (Just v) = reduceSub v

reduceSub :: MonadASM m => Integer -> m ()
reduceSub v = do
  push v
  sub

sub :: MonadASM0 m
sub = dsl Sub

reduceMulOpt :: MonadASM m => Maybe Integer -> m ()
reduceMulOpt Nothing  = mul
reduceMulOpt (Just v) = reduceMul v

reduceMul :: MonadASM m => Integer -> m ()
reduceMul v = do
  push v
  mul

mul :: MonadASM0 m
mul = dsl Mul

reduceDivOpt :: MonadASM m => Maybe Integer -> m ()
reduceDivOpt Nothing  = div
reduceDivOpt (Just v) = reduceDiv v

reduceDiv :: MonadASM m => Integer -> m ()
reduceDiv v = do
  push v
  div

div :: MonadASM0 m
div = dsl Div

reduceModOpt :: MonadASM m => Maybe Integer -> m ()
reduceModOpt Nothing  = mod
reduceModOpt (Just v) = reduceMod v

reduceMod :: MonadASM m => Integer -> m ()
reduceMod v = do
  push v
  mod

mod :: MonadASM0 m
mod = dsl Mod

reduceLoadOpt :: MonadASM m => Maybe Integer -> m ()
reduceLoadOpt Nothing  = load
reduceLoadOpt (Just v) = reduceLoad v

reduceLoad :: MonadASM m => Integer -> m ()
reduceLoad v = do
  push v
  load

load :: MonadASM0 m
load = dsl Load

reduceStoreOpt :: MonadASM m => Maybe Integer -> m ()
reduceStoreOpt Nothing  = store
reduceStoreOpt (Just v) = reduceStore v

reduceStore :: MonadASM m => Integer -> m ()
reduceStore v = do
  push v
  swap
  store

storeVA :: MonadASM m => Integer -> Integer -> m ()
storeVA v a = do
  push v
  reduceStoreA a

reduceStoreA :: MonadASM m => Integer -> m ()
reduceStoreA a = do
  push a
  store

store :: MonadASM0 m
store = dsl Store

mark :: MonadASM1I m
mark = dsl . Mark

branch :: MonadASM1I m
branch = dsl . Branch

branchZ :: MonadASM1I m
branchZ = dsl . BranchZ

branchM :: MonadASM1I m
branchM = dsl . BranchM

branchNZ :: MonadASM1I m
branchNZ l = do
  l1 <- calculateLocalLabel l
  branchZ l1
  branch l
  mark l1

branchNM :: MonadASM1I m
branchNM l = do
  l1 <- calculateLocalLabel l
  branchM l1
  branch l
  mark l1

branchP :: MonadASM1I m
branchP l = do
  l1 <- calculateLocalLabel l
  dup
  branchM l1
  dup
  branchZ l1
  pop
  branch l
  mark l1
  pop

branchNP :: MonadASM1I m
branchNP l = do
  l1 <- calculateLocalLabel l
  l2 <- calculateLocalLabel l
  dup
  branchM l1
  dup
  branchZ l1
  branch l2
  mark l1
  pop
  branch l
  mark l2
  pop

return :: MonadASM0 m
return = dsl Return

end :: MonadASM0 m
end = dsl End

type MonadASM1VO m = MonadASM m => Maybe IntegerValue -> m ()

type MonadASM1V m = MonadASM m => IntegerValue -> m ()

type MonadASM1IO m = MonadASM m => Maybe Identifier -> m ()

type MonadASM1I m = MonadASM m => Identifier -> m ()

type MonadASM0 m = MonadASM m => m ()

type MonadASM m = MonadDSL Instruction m

type Instructions = [Instruction]
