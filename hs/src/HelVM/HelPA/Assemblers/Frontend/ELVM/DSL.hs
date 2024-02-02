module HelVM.HelPA.Assemblers.Frontend.ELVM.DSL where

import           HelVM.HelPA.Assemblers.Common.DSL

newtype Register = R Natural

newtype Immediate = I Integer

unRegister :: Register -> Natural
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

  jeq :: ImmediateORRegister -> ImmediateORRegister -> Register -> DSL a
  jne :: ImmediateORRegister -> ImmediateORRegister -> Register -> DSL a
  jlt :: ImmediateORRegister -> ImmediateORRegister -> Register -> DSL a
  jgt :: ImmediateORRegister -> ImmediateORRegister -> Register -> DSL a
  jle :: ImmediateORRegister -> ImmediateORRegister -> Register -> DSL a
  jge :: ImmediateORRegister -> ImmediateORRegister -> Register -> DSL a

  jmp :: ImmediateORRegister -> Register -> DSL a

  eq :: ImmediateORRegister -> Register -> DSL a
  ne :: ImmediateORRegister -> Register -> DSL a
  lt :: ImmediateORRegister -> Register -> DSL a
  gt :: ImmediateORRegister -> Register -> DSL a
  le :: ImmediateORRegister -> Register -> DSL a
  ge :: ImmediateORRegister -> Register -> DSL a

  dump :: DSL a
