module HelVM.HelPA.Assemblers.Frontend.ELVM.DSL where

import           Control.Monad.RWS.Lazy
import           Data.DList

type Config = ()

type Environment = ()

type Result a = RWS Config (DList a) Environment ()

newtype Register = R Natural

newtype Immediate = I Integer

unRegister :: Register -> Natural
unRegister (R n) = n

unImmediate :: Immediate -> Integer
unImmediate (I i) = i

type ImmediateORRegister = Either Immediate Register

mov :: (DSL a) => ImmediateORRegister -> Register -> Result a
mov (Left i)  = movi i
mov (Right r) = movr r

add :: (DSL a) => ImmediateORRegister -> Register -> Result a
add (Left i)  = addi i
add (Right r) = addr r

sub :: (DSL a) => ImmediateORRegister -> Register -> Result a
sub (Left i)  = subi i
sub (Right r) = subr r

load :: (DSL a) => ImmediateORRegister -> Register -> Result a
load (Left i)  = loadi i
load (Right r) = loadr r

store :: (DSL a) => Register -> ImmediateORRegister -> Result a
store s (Left i)  = storei s i
store s (Right r) = storer s r

putc :: (DSL a) => ImmediateORRegister -> Result a
putc (Left i)  = putci i
putc (Right r) = putcr r

class DSL a where
  movi :: Immediate -> Register -> Result a
  movr :: Register -> Register -> Result a

  addi :: Immediate -> Register -> Result a
  addr :: Register -> Register -> Result a

  subi :: Immediate -> Register -> Result a
  subr :: Register -> Register -> Result a

  loadi :: Immediate -> Register -> Result a
  loadr :: Register -> Register -> Result a

  storei :: Register -> Immediate -> Result a
  storer :: Register -> Register -> Result a

  putci :: Immediate -> Result a
  putcr :: Register -> Result a

  getc :: Register -> Result a

  jeq :: ImmediateORRegister -> ImmediateORRegister -> Register -> Result a
  jne :: ImmediateORRegister -> ImmediateORRegister -> Register -> Result a
  jlt :: ImmediateORRegister -> ImmediateORRegister -> Register -> Result a
  jgt :: ImmediateORRegister -> ImmediateORRegister -> Register -> Result a
  jle :: ImmediateORRegister -> ImmediateORRegister -> Register -> Result a
  jge :: ImmediateORRegister -> ImmediateORRegister -> Register -> Result a

  jmp :: ImmediateORRegister -> Register -> Result a

  eq :: ImmediateORRegister -> Register -> Result a
  ne :: ImmediateORRegister -> Register -> Result a
  lt :: ImmediateORRegister -> Register -> Result a
  gt :: ImmediateORRegister -> Register -> Result a
  le :: ImmediateORRegister -> Register -> Result a
  ge :: ImmediateORRegister -> Register -> Result a

  dump :: Result a
