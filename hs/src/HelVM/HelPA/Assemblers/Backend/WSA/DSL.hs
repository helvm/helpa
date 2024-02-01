module HelVM.HelPA.Assemblers.Backend.WSA.DSL where

import           HelVM.HelPA.Assemblers.Backend.WSA.Instruction

import           HelVM.HelPA.Assembler.Value

import           HelVM.HelIO.Collections.SList

import           Control.Monad.Writer.Lazy

import           Control.Monad.RWS.Lazy

import qualified Data.ListLike                                  as LL

import           Prelude                                        hiding (div, mod, swap)

execDSL :: RWS Natural Instructions Natural a -> InstructionList
execDSL w = snd $ evalRWS w 1 1

reducePushS :: MonadWriter InstructionList m => SString -> m ()
reducePushS = tell . pushLiteralS

pushLiteralS :: SString -> InstructionList
pushLiteralS s = pushLiteral 0 : toList (LL.reverse $ pushLiteralC <$> s)

pushLiteralC :: Char -> Instruction
pushLiteralC = pushLiteral . fromIntegral . ord

push :: MonadDSL1V m
push = dsl . Push

pop :: MonadDSL0 m
pop = dsl Pop

dup :: MonadDSL0 m
dup = dsl Dup

swap :: MonadDSL0 m
swap = dsl Swap

reduceTest :: MonadDSL1V m
reduceTest v = do
  dup
  reduceSub v

reduceAddOpt :: MonadDSL1VO m
reduceAddOpt Nothing  = add
reduceAddOpt (Just v) = reduceAdd v

reduceAdd :: MonadDSL1V m
reduceAdd v = do
  push v
  add

add :: MonadDSL0 m
add = dsl Add

reduceSubOpt :: MonadDSL1VO m
reduceSubOpt Nothing  = sub
reduceSubOpt (Just v) = reduceSub v

reduceSub :: MonadDSL1V m
reduceSub v = do
  push v
  sub

sub :: MonadDSL0 m
sub = dsl Sub

reduceMulOpt :: MonadDSL1VO m
reduceMulOpt Nothing  = mul
reduceMulOpt (Just v) = reduceMul v

reduceMul :: MonadDSL1V m
reduceMul v = do
  push v
  mul

mul :: MonadDSL0 m
mul = dsl Mul

reduceDivOpt :: MonadDSL1VO m
reduceDivOpt Nothing  = div
reduceDivOpt (Just v) = reduceDiv v

reduceDiv :: MonadDSL1V m
reduceDiv v = do
  push v
  div

div :: MonadDSL0 m
div = dsl Div

reduceModOpt :: MonadDSL1VO m
reduceModOpt Nothing  = mod
reduceModOpt (Just v) = reduceMod v

reduceMod :: MonadDSL1V m
reduceMod v = do
  push v
  mod

mod :: MonadDSL0 m
mod = dsl Mod

reduceLoadOpt :: MonadDSL1VO m
reduceLoadOpt Nothing  = load
reduceLoadOpt (Just v) = reduceLoad v

reduceLoad :: MonadDSL1V m
reduceLoad v = do
  push v
  load

load :: MonadDSL0 m
load = dsl Load

reduceStoreOpt :: MonadDSL1VO m
reduceStoreOpt Nothing  = store
reduceStoreOpt (Just v) = reduceStore v

reduceStore :: MonadDSL1V m
reduceStore v = do
  push v
  swap
  store

store :: MonadDSL0 m
store = dsl Store

mark :: MonadDSL1I m
mark = dsl . Mark

branch :: MonadDSL1I m
branch = dsl . Branch

branchZ :: MonadDSL1I m
branchZ = dsl . BranchZ

branchM :: MonadDSL1I m
branchM = dsl . BranchM

reduceBranchNZ :: MonadDSL1I m
reduceBranchNZ l = do
  l1 <- calculateLocalLabelM l
  branchZ l1
  branch l
  mark l1

reduceBranchNM :: MonadDSL1I m
reduceBranchNM l = do
  l1 <- calculateLocalLabelM l
  branchM l1
  branch l
  mark l1

reduceBranchP :: MonadDSL1I m
reduceBranchP l = do
  l1 <- calculateLocalLabelM l
  dup
  branchM l1
  dup
  branchZ l1
  pop
  branch l
  mark l1
  pop

reduceBranchNP :: MonadDSL1I m
reduceBranchNP l = do
  l1 <- calculateLocalLabelM l
  l2 <- calculateLocalLabelM l
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

return :: MonadDSL0 m
return = dsl Return

end :: MonadDSL0 m
end = dsl End

calculateLocalLabelM :: MonadDSL m => Identifier -> m Identifier
calculateLocalLabelM label = do
  suffix <- get
  modify (+ 1)
  pure $ calculateLocalLabel label suffix

calculateLocalLabel :: Identifier -> Natural -> Identifier
calculateLocalLabel label suffix = label <> ":" <> show suffix

dsl :: MonadDSL m => Instruction -> m ()
dsl = tell . LL.singleton

type MonadDSL1VO m = MonadDSL m => Maybe IntegerValue -> m ()

type MonadDSL1V m = MonadDSL m => IntegerValue -> m ()

type MonadDSL1IO m = MonadDSL m => Maybe Identifier ->  m ()

type MonadDSL1I m = MonadDSL m => Identifier ->  m ()

type MonadDSL0 m = MonadDSL m => m ()

type MonadDSL m = MonadRWS Natural Instructions Natural m

type Instructions = [Instruction]
