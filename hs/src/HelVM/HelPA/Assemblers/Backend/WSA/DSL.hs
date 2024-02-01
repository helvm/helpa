module HelVM.HelPA.Assemblers.Backend.WSA.DSL where

import           HelVM.HelPA.Assemblers.Backend.WSA.Instruction

import           HelVM.HelPA.Assembler.Value

import           HelVM.HelIO.Collections.SList

import           Control.Monad.Writer.Lazy

import           Control.Monad.RWS.Lazy

import qualified Data.ListLike                                  as LL

import           Prelude                                        hiding (div, mod, swap)

execDSL :: Natural -> ReaderT Natural (Writer Instructions) () -> InstructionList
execDSL n w = execWriter (runReaderT w n)

reducePushS :: MonadWriter InstructionList m => SString -> m ()
reducePushS = tell . pushLiteralS

pushLiteralS :: SString -> InstructionList
pushLiteralS s = pushLiteral 0 : toList (LL.reverse $ pushLiteralC <$> s)

pushLiteralC :: Char -> Instruction
pushLiteralC = pushLiteral . fromIntegral . ord

push :: DSL1V m
push = dsl . Push

pop :: DSL0 m
pop = dsl Pop

dup :: DSL0 m
dup = dsl Dup

swap :: DSL0 m
swap = dsl Swap

reduceTest :: DSL1V m
reduceTest v = do
  dup
  reduceSub v

reduceAddOpt :: DSL1VO m
reduceAddOpt Nothing  = add
reduceAddOpt (Just v) = reduceAdd v

reduceAdd :: DSL1V m
reduceAdd v = do
  push v
  add

add :: DSL0 m
add = dsl Add

reduceSubOpt :: DSL1VO m
reduceSubOpt Nothing  = sub
reduceSubOpt (Just v) = reduceSub v

reduceSub :: DSL1V m
reduceSub v = do
  push v
  sub

sub :: DSL0 m
sub = dsl Sub

reduceMulOpt :: DSL1VO m
reduceMulOpt Nothing  = mul
reduceMulOpt (Just v) = reduceMul v

reduceMul :: DSL1V m
reduceMul v = do
  push v
  mul

mul :: DSL0 m
mul = dsl Mul

reduceDivOpt :: DSL1VO m
reduceDivOpt Nothing  = div
reduceDivOpt (Just v) = reduceDiv v

reduceDiv :: DSL1V m
reduceDiv v = do
  push v
  div

div :: DSL0 m
div = dsl Div

reduceModOpt :: DSL1VO m
reduceModOpt Nothing  = mod
reduceModOpt (Just v) = reduceMod v

reduceMod :: DSL1V m
reduceMod v = do
  push v
  mod

mod :: DSL0 m
mod = dsl Mod

reduceLoadOpt :: DSL1VO m
reduceLoadOpt Nothing  = load
reduceLoadOpt (Just v) = reduceLoad v

reduceLoad :: DSL1V m
reduceLoad v = do
  push v
  load

load :: DSL0 m
load = dsl Load

reduceStoreOpt :: DSL1VO m
reduceStoreOpt Nothing  = store
reduceStoreOpt (Just v) = reduceStore v

reduceStore :: DSL1V m
reduceStore v = do
  push v
  swap
  store

store :: DSL0 m
store = dsl Store

mark :: DSL1I m
mark = dsl . Mark

branch :: DSL1I m
branch = dsl . Branch

branchZ :: DSL1I m
branchZ = dsl . BranchZ

branchM :: DSL1I m
branchM = dsl . BranchM

reduceBranchNZ :: DSL1I m
reduceBranchNZ l = do
  n <- ask
  let l1 = calculateLocalLabel l n 1
  branchZ l1
  branch l
  mark l1

reduceBranchNM :: DSL1I m
reduceBranchNM l = do
  n <- ask
  let l1 = calculateLocalLabel l n 1
  branchM l1
  branch l
  mark l1

reduceBranchP :: DSL1I m
reduceBranchP l = do
  n <- ask
  let l1 = calculateLocalLabel l n 1
  dup
  branchM l1
  dup
  branchZ l1
  pop
  branch l
  mark l1
  pop

reduceBranchNP :: DSL1I m
reduceBranchNP l = do
  n <- ask
  let l1 = calculateLocalLabel l n 1
  let l2 = calculateLocalLabel l n 2
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

return :: DSL0 m
return = dsl Return

end :: DSL0 m
end = dsl End

calculateLocalLabel :: Identifier -> Natural -> Natural -> Identifier
calculateLocalLabel label suffix suffix2 = label <> ":" <> show suffix <> ":" <> show suffix2

dsl :: DSL m => Instruction -> m ()
dsl = tell . LL.singleton

type DSL1VO m = DSL m => Maybe IntegerValue -> m ()

type DSL1V m = DSL m => IntegerValue -> m ()

type DSL1IO m = DSL m => Maybe Identifier ->  m ()

type DSL1I m = DSL m => Identifier ->  m ()

type DSL0 m = DSL m => m ()

type DSL m = (MonadWriter Instructions m , MonadReader Natural m)

type Instructions = [Instruction]
