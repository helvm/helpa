module HelVM.HelPA.Assemblers.Frontend.ELVM.DSL where

import           HelVM.HelPA.Assemblers.Common.DSL

import qualified HelVM.HelPA.Assemblers.Backend.WSA.ExtendInstruction as WSA

mov :: (ELVM a , MonadDSL a m) => ImmediateORRegister -> Register -> m ()
mov (Left i)  = movi i
mov (Right r) = movr r

add :: (ELVM a , MonadDSL a m) => ImmediateORRegister -> Register -> m ()
add (Left i)  = addi i
add (Right r) = addr r

sub :: (ELVM a , MonadDSL a m) => ImmediateORRegister -> Register -> m ()
sub (Left i)  = subi i
sub (Right r) = subr r

load :: (ELVM a , MonadDSL a m) => ImmediateORRegister -> Register -> m ()
load (Left i)  = loadi i
load (Right r) = loadr r

store :: (ELVM a , MonadDSL a m) => Register -> ImmediateORRegister -> m ()
store s (Left i)  = storei s i
store s (Right r) = storer s r

putc :: (ELVM a , MonadDSL a m) => ImmediateORRegister -> m ()
putc (Left i)  = putci i
putc (Right r) = putcr r

jeq :: (ELVM a , MonadDSL a m) => ImmediateORRegister -> ImmediateORRegister -> Register -> m ()
jeq (Left i1) (Left i2)   = jeqii i1 i2
jeq (Left i1) (Right r2)  = jeqir i1 r2
jeq (Right r1) (Left i2)  = jeqri r1 i2
jeq (Right r1) (Right r2) = jeqrr r1 r2

jne :: (ELVM a , MonadDSL a m) => ImmediateORRegister -> ImmediateORRegister -> Register -> m ()
jne (Left i1) (Left i2)   = jneii i1 i2
jne (Left i1) (Right r2)  = jneir i1 r2
jne (Right r1) (Left i2)  = jneri r1 i2
jne (Right r1) (Right r2) = jnerr r1 r2

jlt :: (ELVM a , MonadDSL a m) => ImmediateORRegister -> ImmediateORRegister -> Register -> m ()
jlt (Left i1) (Left i2)   = jltii i1 i2
jlt (Left i1) (Right r2)  = jltir i1 r2
jlt (Right r1) (Left i2)  = jltri r1 i2
jlt (Right r1) (Right r2) = jltrr r1 r2

jgt :: (ELVM a , MonadDSL a m) => ImmediateORRegister -> ImmediateORRegister -> Register -> m ()
jgt (Left i1) (Left i2)   = jgtii i1 i2
jgt (Left i1) (Right r2)  = jgtir i1 r2
jgt (Right r1) (Left i2)  = jgtri r1 i2
jgt (Right r1) (Right r2) = jgtrr r1 r2

jle :: (ELVM a , MonadDSL a m) => ImmediateORRegister -> ImmediateORRegister -> Register -> m ()
jle (Left i1) (Left i2)   = jleii i1 i2
jle (Left i1) (Right r2)  = jleir i1 r2
jle (Right r1) (Left i2)  = jleri r1 i2
jle (Right r1) (Right r2) = jlerr r1 r2

jge :: (ELVM a , MonadDSL a m) => ImmediateORRegister -> ImmediateORRegister -> Register -> m ()
jge (Left i1) (Left i2)   = jgeii i1 i2
jge (Left i1) (Right r2)  = jgeir i1 r2
jge (Right r1) (Left i2)  = jgeri r1 i2
jge (Right r1) (Right r2) = jgerr r1 r2

jmp :: (ELVM a , MonadDSL a m) => ImmediateORRegister -> m ()
jmp (Left i)  = jmpi i
jmp (Right r) = jmpr r

eq :: (ELVM a , MonadDSL a m) => ImmediateORRegister -> Register -> m ()
eq (Left i)  = eqi i
eq (Right r) = eqr r

ne :: (ELVM a , MonadDSL a m) => ImmediateORRegister -> Register -> m ()
ne (Left i)  = nei i
ne (Right r) = ner r

lt :: (ELVM a , MonadDSL a m) => ImmediateORRegister -> Register -> m ()
lt (Left i)  = lti i
lt (Right r) = ltr r

gt :: (ELVM a , MonadDSL a m) => ImmediateORRegister -> Register -> m ()
gt (Left i)  = gti i
gt (Right r) = gtr r

le :: (ELVM a , MonadDSL a m) => ImmediateORRegister -> Register -> m ()
le (Left i)  = lei i
le (Right r) = ler r

ge :: (ELVM a , MonadDSL a m) => ImmediateORRegister -> Register -> m ()
ge (Left i)  = gei i
ge (Right r) = ger r

class ELVM a where
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

  jeqii :: MonadDSL a m => Immediate -> Immediate -> Register -> m ()
  jeqir :: MonadDSL a m => Immediate -> Register -> Register -> m ()
  jeqri :: MonadDSL a m => Register -> Immediate -> Register -> m ()
  jeqrr :: MonadDSL a m => Register -> Register -> Register -> m ()

  jneii :: MonadDSL a m => Immediate -> Immediate -> Register -> m ()
  jneir :: MonadDSL a m => Immediate -> Register -> Register -> m ()
  jneri :: MonadDSL a m => Register -> Immediate -> Register -> m ()
  jnerr :: MonadDSL a m => Register -> Register -> Register -> m ()

  jltii :: MonadDSL a m => Immediate -> Immediate -> Register -> m ()
  jltir :: MonadDSL a m => Immediate -> Register -> Register -> m ()
  jltri :: MonadDSL a m => Register -> Immediate -> Register -> m ()
  jltrr :: MonadDSL a m => Register -> Register -> Register -> m ()

  jgtii :: MonadDSL a m => Immediate -> Immediate -> Register -> m ()
  jgtir :: MonadDSL a m => Immediate -> Register -> Register -> m ()
  jgtri :: MonadDSL a m => Register -> Immediate -> Register -> m ()
  jgtrr :: MonadDSL a m => Register -> Register -> Register -> m ()

  jleii :: MonadDSL a m => Immediate -> Immediate -> Register -> m ()
  jleir :: MonadDSL a m => Immediate -> Register -> Register -> m ()
  jleri :: MonadDSL a m => Register -> Immediate -> Register -> m ()
  jlerr :: MonadDSL a m => Register -> Register -> Register -> m ()

  jgeii :: MonadDSL a m => Immediate -> Immediate -> Register -> m ()
  jgeir :: MonadDSL a m => Immediate -> Register -> Register -> m ()
  jgeri :: MonadDSL a m => Register -> Immediate -> Register -> m ()
  jgerr :: MonadDSL a m => Register -> Register -> Register -> m ()

  jmpi :: MonadDSL a m => Immediate -> m ()
  jmpr :: MonadDSL a m => Register -> m ()

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

  addi i r = do
    movi i acc
    addr acc r

  subi i r = do
    movi i acc
    subr acc r

  loadi i r = do
    movi i acc
    loadr acc r

  storei r i = do
    movi i acc
    storer acc r

  putci i = do
    movi i acc
    putcr acc

  jeqii i0 i1 r2 = do
    movi i0 acc
    movi i1 acc1
    jeqrr acc acc1 r2

  jeqir i0 r1 r2 = do
    movi i0 acc
    jeqrr acc r1 r2

  jeqri r0 i1 r2 = do
    movi i1 acc
    jeqrr r0 acc r2

  jneii i0 i1 r2 = do
    movi i0 acc
    movi i1 acc1
    jnerr acc acc1 r2

  jneir i0 r1 r2 = do
    movi i0 acc
    jnerr acc r1 r2

  jneri r0 i1 r2 = do
    movi i1 acc
    jnerr r0 acc r2

  jltii i0 i1 r2 = do
    movi i0 acc
    movi i1 acc1
    jltrr acc acc1 r2

  jltir i0 r1 r2 = do
    movi i0 acc
    jltrr acc r1 r2

  jltri r0 i1 r2 = do
    movi i1 acc
    jltrr r0 acc r2

  jgtii i0 i1 r2 = do
    movi i0 acc
    movi i1 acc1
    jgtrr acc acc1 r2

  jgtir i0 r1 r2 = do
    movi i0 acc
    jgtrr acc r1 r2

  jgtri r0 i1 r2 = do
    movi i1 acc
    jgtrr r0 acc r2

  jleii i0 i1 r2 = do
    movi i0 acc
    movi i1 acc1
    jlerr acc acc1 r2

  jleir i0 r1 r2 = do
    movi i0 acc
    jlerr acc r1 r2

  jleri r0 i1 r2 = do
    movi i1 acc
    jlerr r0 acc r2

  jgeii i0 i1 r2 = do
    movi i0 acc
    movi i1 acc1
    jgerr acc acc1 r2

  jgeir i0 r1 r2 = do
    movi i0 acc
    jgerr acc r1 r2

  jgeri r0 i1 r2 = do
    movi i1 acc
    jgerr r0 acc r2

  jmpi i = do
    movi i acc
    jmpr acc

  eqi i r = do
    movi i acc
    eqr acc r

  nei i r = do
    movi i acc
    ner acc r

  lti i r = do
    movi i acc
    ltr acc r

  gti i r = do
    movi i acc
    gtr acc r

  lei i r = do
    movi i acc
    ler acc r

  gei i r = do
    movi i acc
    ger acc r

instance ELVM WSA.ExtendInstruction where
  movi = WSA.movi
  movr = WSA.movr
  addr = WSA.addr
  subr = WSA.subr
  loadr = WSA.loadr
  storer = WSA.storer
  putcr = WSA.putcr
  getc = WSA.getc
  jeqrr = WSA.jeqrr
  jnerr = WSA.jnerr
  jltrr = WSA.jltrr
  jgtrr = WSA.jgtrr
  jlerr = WSA.jlerr
  jgerr = WSA.jgerr
  jmpr = WSA.jmpr
  eqr = WSA.eqr
  ner = WSA.ner
  ltr = WSA.ltr
  gtr = WSA.gtr
  ler = WSA.ler
  ger = WSA.ger
