module HelVM.HelPA.Assemblers.Frontend.ELVM.DSL where

import           HelVM.HelPA.Assemblers.Common.DSL

--import qualified HelVM.HelPA.Assemblers.Backend.WSA.Instruction as WSA

newtype Register = R Text

acc :: Register
acc = R ""

acc1 :: Register
acc1 = R "1"

newtype Immediate = I Integer

unRegister :: Register -> Text
unRegister (R n) = n

unImmediate :: Immediate -> Integer
unImmediate (I i) = i

type ImmediateORRegister = Either Immediate Register

mov :: (MonadDSL a) => ImmediateORRegister -> Register -> DSL a
mov (Left i)  = movi i
mov (Right r) = movr r

add :: (MonadDSL a) => ImmediateORRegister -> Register -> DSL a
add (Left i)  = addi i
add (Right r) = addr r

sub :: (MonadDSL a) => ImmediateORRegister -> Register -> DSL a
sub (Left i)  = subi i
sub (Right r) = subr r

load :: (MonadDSL a) => ImmediateORRegister -> Register -> DSL a
load (Left i)  = loadi i
load (Right r) = loadr r

store :: (MonadDSL a) => Register -> ImmediateORRegister -> DSL a
store s (Left i)  = storei s i
store s (Right r) = storer s r

putc :: (MonadDSL a) => ImmediateORRegister -> DSL a
putc (Left i)  = putci i
putc (Right r) = putcr r

jeq :: (MonadDSL a) => ImmediateORRegister -> ImmediateORRegister -> Register -> DSL a
jeq (Left i1) (Left i2)   = jeqii i1 i2
jeq (Left i1) (Right r2)  = jeqir i1 r2
jeq (Right r1) (Left i2)  = jeqri r1 i2
jeq (Right r1) (Right r2) = jeqrr r1 r2

jne :: (MonadDSL a) => ImmediateORRegister -> ImmediateORRegister -> Register -> DSL a
jne (Left i1) (Left i2)   = jneii i1 i2
jne (Left i1) (Right r2)  = jneir i1 r2
jne (Right r1) (Left i2)  = jneri r1 i2
jne (Right r1) (Right r2) = jnerr r1 r2

jlt :: (MonadDSL a) => ImmediateORRegister -> ImmediateORRegister -> Register -> DSL a
jlt (Left i1) (Left i2)   = jltii i1 i2
jlt (Left i1) (Right r2)  = jltir i1 r2
jlt (Right r1) (Left i2)  = jltri r1 i2
jlt (Right r1) (Right r2) = jltrr r1 r2

jgt :: (MonadDSL a) => ImmediateORRegister -> ImmediateORRegister -> Register -> DSL a
jgt (Left i1) (Left i2)   = jgtii i1 i2
jgt (Left i1) (Right r2)  = jgtir i1 r2
jgt (Right r1) (Left i2)  = jgtri r1 i2
jgt (Right r1) (Right r2) = jgtrr r1 r2

jle :: (MonadDSL a) => ImmediateORRegister -> ImmediateORRegister -> Register -> DSL a
jle (Left i1) (Left i2)   = jleii i1 i2
jle (Left i1) (Right r2)  = jleir i1 r2
jle (Right r1) (Left i2)  = jleri r1 i2
jle (Right r1) (Right r2) = jlerr r1 r2

jge :: (MonadDSL a) => ImmediateORRegister -> ImmediateORRegister -> Register -> DSL a
jge (Left i1) (Left i2)   = jgeii i1 i2
jge (Left i1) (Right r2)  = jgeir i1 r2
jge (Right r1) (Left i2)  = jgeri r1 i2
jge (Right r1) (Right r2) = jgerr r1 r2

jmp :: (MonadDSL a) => ImmediateORRegister -> Register -> DSL a
jmp (Left i)  = jmpi i
jmp (Right r) = jmpr r

eq :: (MonadDSL a) => ImmediateORRegister -> Register -> DSL a
eq (Left i)  = eqi i
eq (Right r) = eqr r

ne :: (MonadDSL a) => ImmediateORRegister -> Register -> DSL a
ne (Left i)  = nei i
ne (Right r) = ner r

lt :: (MonadDSL a) => ImmediateORRegister -> Register -> DSL a
lt (Left i)  = lti i
lt (Right r) = ltr r

gt :: (MonadDSL a) => ImmediateORRegister -> Register -> DSL a
gt (Left i)  = gti i
gt (Right r) = gtr r

le :: (MonadDSL a) => ImmediateORRegister -> Register -> DSL a
le (Left i)  = lei i
le (Right r) = ler r

ge :: (MonadDSL a) => ImmediateORRegister -> Register -> DSL a
ge (Left i)  = gei i
ge (Right r) = ger r

class MonadDSL a where
  movi :: Immediate -> Register -> DSL a
  movr :: Register -> Register -> DSL a

  addi :: Immediate -> Register -> DSL a
  addr :: Register -> Register -> DSL a

  subi :: Immediate -> Register -> DSL a
  subr :: Register -> Register -> DSL a

  loadi :: Immediate -> Register -> DSL a
  loadr :: Register -> Register -> DSL a

  storei :: Register -> Immediate -> DSL a
  storer :: Register -> Register -> DSL a

  putci :: Immediate -> DSL a
  putcr :: Register -> DSL a

  getc :: Register -> DSL a

  jeqii :: Immediate -> Immediate -> Register -> DSL a
  jeqir :: Immediate -> Register -> Register -> DSL a
  jeqri :: Register -> Immediate -> Register -> DSL a
  jeqrr :: Register -> Register -> Register -> DSL a

  jneii :: Immediate -> Immediate -> Register -> DSL a
  jneir :: Immediate -> Register -> Register -> DSL a
  jneri :: Register -> Immediate -> Register -> DSL a
  jnerr :: Register -> Register -> Register -> DSL a

  jltii :: Immediate -> Immediate -> Register -> DSL a
  jltir :: Immediate -> Register -> Register -> DSL a
  jltri :: Register -> Immediate -> Register -> DSL a
  jltrr :: Register -> Register -> Register -> DSL a

  jgtii :: Immediate -> Immediate -> Register -> DSL a
  jgtir :: Immediate -> Register -> Register -> DSL a
  jgtri :: Register -> Immediate -> Register -> DSL a
  jgtrr :: Register -> Register -> Register -> DSL a

  jleii :: Immediate -> Immediate -> Register -> DSL a
  jleir :: Immediate -> Register -> Register -> DSL a
  jleri :: Register -> Immediate -> Register -> DSL a
  jlerr :: Register -> Register -> Register -> DSL a

  jgeii :: Immediate -> Immediate -> Register -> DSL a
  jgeir :: Immediate -> Register -> Register -> DSL a
  jgeri :: Register -> Immediate -> Register -> DSL a
  jgerr :: Register -> Register -> Register -> DSL a

  jmpi :: Immediate -> Register -> DSL a
  jmpr :: Register -> Register -> DSL a

  eqi :: Immediate -> Register -> DSL a
  eqr :: Register -> Register -> DSL a

  nei :: Immediate -> Register -> DSL a
  ner :: Register -> Register -> DSL a

  lti :: Immediate -> Register -> DSL a
  ltr :: Register -> Register -> DSL a

  gti :: Immediate -> Register -> DSL a
  gtr :: Register -> Register -> DSL a

  lei :: Immediate -> Register -> DSL a
  ler :: Register -> Register -> DSL a

  gei :: Immediate -> Register -> DSL a
  ger :: Register -> Register -> DSL a

  dump :: DSL a

  addi i r = do
    movi i acc
    addr acc r

  subi i r = do
    movi i acc
    subr acc r

  loadi i r = do
    movi i acc
    loadr acc r

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

  jmpi i r = do
    movi i acc
    jmpr acc r

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


--instance DSL WSA.Instruction where
--  movi s d = do
--    push $ Value $ unImmediate s
--
