module HelVM.HelPA.Assemblers.Backend.WSA.ExtendInstruction where

import           HelVM.HelPA.Assemblers.Backend.WSA.Instruction

import           HelVM.HelPA.Assemblers.Common.DSL

import           HelVM.HelPA.Assembler.Value

import           Control.Monad.RWS.Lazy

import qualified HelVM.HelPA.Assemblers.Backend.WSA.DSL         as WSA

movi :: MonadExtendASM m => Immediate -> Register ->  m ()
movi s d = do
  basic $ Push $ Literal $ unImmediate s
  loadRegister d
  store

movr :: MonadExtendASM m => Register -> Register ->  m ()
movr s d = do
  loadRegister s
  loadRegister d
  store

addr :: MonadExtendASM m => Register -> Register ->  m ()
addr s d = do
  loadRegister s
  loadRegister d
  basic Add
  storeRegister d

subr :: MonadExtendASM m => Register -> Register ->  m ()
subr s d = do
  loadRegister s
  loadRegister d
  sub
  storeRegister d

loadr :: MonadExtendASM m => Register -> Register ->  m ()
loadr s d = do
  loadRegister s
  loadRegister d
  basic Load
  loadRegister d

storer :: MonadExtendASM m => Register -> Register ->  m ()
storer s d = do
  loadRegister s
  loadRegister d
  store

putcr :: MonadExtendASM m => Register ->  m ()
putcr s = do
  loadRegister s
  basic OutputChar

getc :: MonadExtendASM m => Register ->  m ()
getc d = do
  basic InputChar
  storeRegister d

jeqrr :: MonadExtendASM m => Register -> Register -> Register -> m ()
jeqrr j s d = do
  loadRegister s
  loadRegister d
  sub
  l <- calculateLabel
  extend $ WSA.branchNZ l
  jumpRegister j
  mark l

jnerr :: MonadExtendASM m => Register -> Register -> Register -> m ()
jnerr j s d = do
  loadRegister s
  loadRegister d
  sub
  l <- calculateLabel
  extend $ WSA.branchZ l
  jumpRegister j
  mark l

-- <=
jltrr :: MonadExtendASM m => Register -> Register -> Register -> m ()
jltrr j s d = do
  loadRegister s
  loadRegister d
  sub
  l <- calculateLabel
  extend $ WSA.branchNM l
  jumpRegister j
  mark l

-- >= 0 0
jgtrr :: MonadExtendASM m => Register -> Register -> Register -> m ()
jgtrr j s d = do
  loadRegister s
  loadRegister d
  sub
  l <- calculateLabel
  extend $ WSA.branchNP l
  jumpRegister j
  mark l

-- <
jlerr :: MonadExtendASM m => Register -> Register -> Register -> m ()
jlerr j s d = do
  loadRegister s
  loadRegister d
  sub
  l <- calculateLabel
  extend $ WSA.branchP l
  jumpRegister j
  mark l

-- >
jgerr :: MonadExtendASM m => Register -> Register -> Register -> m ()
jgerr j s d = do
  loadRegister s
  loadRegister d
  sub
  l <- calculateLabel
  extend $ WSA.branchM l
  jumpRegister j
  mark l

jmpr :: MonadExtendASM m => Register -> m ()
jmpr = jumpRegister

eqr :: MonadExtendASM m => Register -> Register -> m ()
eqr s d = do
  loadRegister s
  loadRegister d
  sub
  movi0 d
  l <- calculateLabel
  extend $ WSA.branchNZ l
  movi1 d
  mark l

ner :: MonadExtendASM m => Register -> Register -> m ()
ner s d = do
  loadRegister s
  loadRegister d
  sub
  movi0 d
  l <- calculateLabel
  extend $ WSA.branchZ l
  movi1 d
  mark l

ltr :: MonadExtendASM m => Register -> Register -> m ()
ltr s d = do
  loadRegister s
  loadRegister d
  sub
  movi0 d
  l <- calculateLabel
  extend $ WSA.branchNM l
  movi1 d
  mark l

gtr :: MonadExtendASM m => Register -> Register -> m ()
gtr s d = do
  loadRegister s
  loadRegister d
  sub
  movi0 d
  l <- calculateLabel
  extend $ WSA.branchM l
  movi1 d
  mark l

ler :: MonadExtendASM m => Register -> Register -> m ()
ler s d = do
  loadRegister s
  loadRegister d
  sub
  movi0 d
  l <- calculateLabel
  extend $ WSA.branchP l
  movi1 d
  mark l

ger :: MonadExtendASM m => Register -> Register -> m ()
ger s d = do
  loadRegister s
  loadRegister d
  sub
  movi0 d
  l <- calculateLabel
  extend $ WSA.branchM l
  movi1 d
  mark l

--

movi1 :: MonadExtendASM m => Register -> m ()
movi1 = movi (I 1)

movi0 :: MonadExtendASM m => Register -> m ()
movi0 = movi (I 0)

store :: MonadExtendASM m => m ()
store = basic Store

mark :: MonadExtendASM m => Identifier -> m ()
mark = basic <$> Mark

sub :: MonadExtendASM m => m ()
sub = basic Sub

basic :: MonadExtendASM m => Instruction -> m ()
basic = dsl <$> Basic

loadRegister :: MonadExtendASM m => Register -> m ()
loadRegister = dsl <$> LoadRegister

storeRegister :: MonadExtendASM m => Register -> m ()
storeRegister = dsl <$> StoreRegister

jumpRegister :: MonadExtendASM m => Register -> m ()
jumpRegister = dsl <$> JumpRegister

extend :: MonadExtendASM m => DSL Instruction -> m ()
extend b = do
  r <- ask
  s <- get
  let (a, s1, w) = runRWS b r s
  tell $ Basic <$> w
  put s1
  pure a

type MonadExtendASM m = MonadDSL ExtendInstruction m

data ExtendInstruction =
    Basic Instruction
  | LoadRegister Register
  | StoreRegister Register
  | JumpRegister Register
