module HelVM.HelPA.Assemblers.Frontend.EIR.DSL where

import           HelVM.HelPA.Assemblers.Common.DSL

import           HelVM.HelPA.Assembler.Value

import qualified HelVM.HelPA.Assemblers.Backend.WSA.ExtendInstruction as WSA

import           Control.Monad.RWS.Lazy

mov :: (EIR a , MonadDSL a m) => ImmediateORRegister -> Register -> m ()
mov (Literal i)  = movi i
mov (Variable r) = movr r

add :: (EIR a , MonadDSL a m) => ImmediateORRegister -> Register -> m ()
add (Literal i)  = addi i
add (Variable r) = addr r

sub :: (EIR a , MonadDSL a m) => ImmediateORRegister -> Register -> m ()
sub (Literal i)  = subi i
sub (Variable r) = subr r

load :: (EIR a , MonadDSL a m) => ImmediateORRegister -> Register -> m ()
load (Literal i)  = loadi i
load (Variable r) = loadr r

store :: (EIR a , MonadDSL a m) => Register -> ImmediateORRegister -> m ()
store s (Literal i)  = storei s i
store s (Variable r) = storer s r

putc :: (EIR a , MonadDSL a m) => ImmediateORRegister -> m ()
putc (Literal i)  = putci i
putc (Variable r) = putcr r

jeq :: (EIR a , MonadDSL a m) => ImmediateORRegister -> Register -> Identifier -> m ()
jeq (Literal i)  = jeqi i
jeq (Variable r) = jeqr r

jne :: (EIR a , MonadDSL a m) => ImmediateORRegister -> Register -> Identifier -> m ()
jne (Literal i)  = jnei i
jne (Variable r) = jner r

jlt :: (EIR a , MonadDSL a m) => ImmediateORRegister -> Register -> Identifier -> m ()
jlt (Literal i)  = jlti i
jlt (Variable r) = jltr r

jgt :: (EIR a , MonadDSL a m) => ImmediateORRegister -> Register -> Identifier -> m ()
jgt (Literal i)  = jgti i
jgt (Variable r) = jgtr r

jle :: (EIR a , MonadDSL a m) => ImmediateORRegister -> Register -> Identifier -> m ()
jle (Literal i)  = jlei i
jle (Variable r) = jler r

jge :: (EIR a , MonadDSL a m) => ImmediateORRegister -> Register -> Identifier -> m ()
jge (Literal i)  = jgei i
jge (Variable r) = jger r

eq :: (EIR a , MonadDSL a m) => ImmediateORRegister -> Register -> m ()
eq (Literal i)  = eqi i
eq (Variable r) = eqr r

ne :: (EIR a , MonadDSL a m) => ImmediateORRegister -> Register -> m ()
ne (Literal i)  = nei i
ne (Variable r) = ner r

lt :: (EIR a , MonadDSL a m) => ImmediateORRegister -> Register -> m ()
lt (Literal i)  = lti i
lt (Variable r) = ltr r

gt :: (EIR a , MonadDSL a m) => ImmediateORRegister -> Register -> m ()
gt (Literal i)  = gti i
gt (Variable r) = gtr r

le :: (EIR a , MonadDSL a m) => ImmediateORRegister -> Register -> m ()
le (Literal i)  = lei i
le (Variable r) = ler r

ge :: (EIR a , MonadDSL a m) => ImmediateORRegister -> Register -> m ()
ge (Literal i)  = gei i
ge (Variable r) = ger r

class EIR a where
  movi :: MonadDSL a m => Immediate -> Register -> m ()
  movr :: MonadDSL a m => Register -> Register -> m ()

  addi :: MonadDSL a m => Immediate -> Register -> m ()
  addr :: MonadDSL a m => Register -> Register -> m ()

  subi :: MonadDSL a m => Immediate -> Register -> m ()
  subr :: MonadDSL a m => Register -> Register -> m ()

  loadi :: MonadDSL a m => Immediate -> Register -> m ()
  loadr :: MonadDSL a m => Register -> Register -> m ()

  storei :: MonadDSL a m => Register -> Immediate -> m ()
  storer :: MonadDSL a m => Register -> Register -> m ()

  putci :: MonadDSL a m => Immediate -> m ()
  putcr :: MonadDSL a m => Register -> m ()

  getc :: MonadDSL a m => Register -> m ()

  jeqi :: MonadDSL a m => Immediate -> Register -> Identifier -> m ()
  jeqr :: MonadDSL a m => Register -> Register -> Identifier -> m ()

  jnei :: MonadDSL a m => Immediate -> Register -> Identifier -> m ()
  jner :: MonadDSL a m => Register -> Register -> Identifier -> m ()

  jlti :: MonadDSL a m => Immediate -> Register -> Identifier -> m ()
  jltr :: MonadDSL a m => Register -> Register -> Identifier -> m ()

  jgti :: MonadDSL a m => Immediate -> Register -> Identifier -> m ()
  jgtr :: MonadDSL a m => Register -> Register -> Identifier -> m ()

  jlei :: MonadDSL a m => Immediate -> Register -> Identifier -> m ()
  jler :: MonadDSL a m => Register -> Register -> Identifier -> m ()

  jgei :: MonadDSL a m => Immediate -> Register -> Identifier -> m ()
  jger :: MonadDSL a m => Register -> Register -> Identifier -> m ()

  jmp :: MonadDSL a m => Identifier -> m ()

  eqi :: MonadDSL a m => Immediate -> Register -> m ()
  eqr :: MonadDSL a m => Register -> Register -> m ()

  nei :: MonadDSL a m => Immediate -> Register -> m ()
  ner :: MonadDSL a m => Register -> Register -> m ()

  lti :: MonadDSL a m => Immediate -> Register -> m ()
  ltr :: MonadDSL a m => Register -> Register -> m ()

  gti :: MonadDSL a m => Immediate -> Register -> m ()
  gtr :: MonadDSL a m => Register -> Register -> m ()

  lei :: MonadDSL a m => Immediate -> Register -> m ()
  ler :: MonadDSL a m => Register -> Register -> m ()

  gei :: MonadDSL a m => Immediate -> Register -> m ()
  ger :: MonadDSL a m => Register -> Register -> m ()

  dump :: MonadDSL a m => m ()
  mark :: MonadDSL a m => Identifier -> m ()
  pText :: MonadDSL a m => m ()
  pData :: MonadDSL a m => Maybe Natural -> m ()
  pLong :: MonadDSL a m => Integer -> m ()
  pString :: MonadDSL a m => Text -> m ()
  pFile :: MonadDSL a m => Natural -> Identifier  -> m ()
  pLoc :: MonadDSL a m => Natural -> Natural -> Natural -> m ()

  addi i r = do
    movi i temp0
    addr temp0 r

  subi i r = do
    movi i temp0
    subr temp0 r

  loadi i r = do
    movi i temp0
    loadr temp0 r

  storei r i = do
    movi i temp0
    storer temp0 r

  putci i = do
    movi i temp0
    putcr temp0

  jeqi i r j = do
    movi i temp0
    jeqr temp0 r j

  jnei i r j = do
    movi i temp0
    jner temp0 r j

  jlti i r j = do
    movi i temp0
    jltr temp0 r j

  jgti i r j = do
    movi i temp0
    jgtr temp0 r j

  jlei i r j = do
    movi i temp0
    jler temp0 r j

  jgei i r j = do
    movi i temp0
    jger temp0 r j

  eqi i r = do
    movi i temp0
    eqr temp0 r

  nei i r = do
    movi i temp0
    ner temp0 r

  lti i r = do
    movi i temp0
    ltr temp0 r

  gti i r = do
    movi i temp0
    gtr temp0 r

  lei i r = do
    movi i temp0
    ler temp0 r

  gei i r = do
    movi i temp0
    ger temp0 r

  dump = tell []
  pText = dump
  pData _ = dump
  pLong _ = dump
  pString _ = dump
  pFile _ _= dump
  pLoc _ _ _= dump

instance EIR WSA.ExtendInstruction where
  movi = WSA.movi
  movr = WSA.movr
  addr = WSA.addr
  subr = WSA.subr
  loadr = WSA.loadr
  storer = WSA.storer
  putcr = WSA.putcr
  getc = WSA.getc
  jeqr = WSA.jeqr
  jner = WSA.jner
  jltr = WSA.jltr
  jgtr = WSA.jgtr
  jler = WSA.jler
  jger = WSA.jger
  jmp = WSA.jmp
  eqr = WSA.eqr
  ner = WSA.ner
  ltr = WSA.ltr
  gtr = WSA.gtr
  ler = WSA.ler
  ger = WSA.ger
  mark = WSA.mark
--  pLong = WSA.pLong
