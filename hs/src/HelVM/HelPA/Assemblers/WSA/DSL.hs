module HelVM.HelPA.Assemblers.WSA.DSL where

import           HelVM.HelPA.Assemblers.WSA.Instruction

import           HelVM.HelPA.Assembler.Value

import           Control.Monad.Writer.Lazy

import Control.Monad.RWS.Lazy

import qualified Data.ListLike                          as LL

import           Prelude                                hiding (div, mod , swap)

execDSL :: Natural -> ReaderT Natural (Writer Instructions) () -> InstructionList
execDSL n w = execWriter (runReaderT w n)

reduceTest :: DSL1V m
reduceTest v = do
  dup
  reduceSub v

reduceSub :: DSL1V m
reduceSub v = do
  push v
  sub

reduceAdd :: DSL1V m
reduceAdd v = do
  push v
  add

reduceMul :: DSL1V m
reduceMul v = do
  push v
  mul

reduceDiv :: DSL1V m
reduceDiv v = do
  push v
  div

reduceMod :: DSL1V m
reduceMod v = do
  push v
  mod

reduceLoad :: DSL1V m
reduceLoad v = do
  push v
  load

reduceStore :: DSL1V m
reduceStore v = do
  push v
  swap
  store

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

add :: DSL0 m
add = dsl $ Add Nothing

sub :: DSL0 m
sub = dsl $ Sub Nothing

mul :: DSL0 m
mul = dsl $ Mul Nothing

div :: DSL0 m
div = dsl $ Div Nothing

mod :: DSL0 m
mod = dsl $ Mod Nothing

load :: DSL0 m
load = dsl $ Load Nothing

swap :: DSL0 m
swap = dsl Swap

push :: DSL1V m
push = dsl . Push

store :: DSL0 m
store = dsl $ Store Nothing

branchZ :: DSL1I m
branchZ = dsl . BranchZ

branchM :: DSL1I m
branchM = dsl . BranchM

branch :: DSL1I m
branch = dsl . Branch

mark :: DSL1I m
mark = dsl . Mark

dup :: DSL0 m
dup = dsl Dup

pop :: DSL0 m
pop = dsl Pop

calculateLocalLabel :: Identifier -> Natural -> Natural -> Identifier
calculateLocalLabel label suffix suffix2 = label <> ":" <> show suffix <> ":" <> show suffix2

dsl :: DSL m => Instruction -> m ()
dsl = tell . LL.singleton

type DSL1V m = DSL m => IntegerValue -> m ()

type DSL1I m = DSL m => Identifier ->  m ()

type DSL0 m = DSL m => m ()

type DSL m = (MonadWriter Instructions m , MonadReader Natural m)

type Instructions = [Instruction]
