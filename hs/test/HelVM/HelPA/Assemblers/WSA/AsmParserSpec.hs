module HelVM.HelPA.Assemblers.WSA.AsmParserSpec (spec) where

import HelVM.HelPA.Assemblers.WSA.AsmParser
import HelVM.HelPA.Assemblers.WSA.Instruction

import HelVM.HelPA.Assemblers.WSA.TestData
import HelVM.HelPA.Assemblers.WSA.FileUtil
import HelVM.HelPA.Assemblers.Util

import HelVM.HelPA.Common.Value

import Data.List

import Test.Hspec
import Test.Hspec.Attoparsec

spec :: Spec
spec = do
  describe "Commands without operands" $ do
    it "parse 'pop'"  $ do parseAssembler "pop" `shouldParse` [Pop]
    it "parse 'doub'" $ do parseAssembler "doub" `shouldParse` [Dup]
    it "parse 'swap'" $ do parseAssembler "swap" `shouldParse` [Swap]
    it "parse 'ret'"  $ do parseAssembler "ret" `shouldParse` [Return]
    it "parse 'exit'" $ do parseAssembler "exit" `shouldParse` [End]
    it "parse 'outn'" $ do parseAssembler "outn" `shouldParse` [OutputNum]
    it "parse 'outc'" $ do parseAssembler "outc" `shouldParse` [OutputChar]
    it "parse 'inn'"  $ do parseAssembler "inn" `shouldParse` [InputNum]
    it "parse 'inc'"  $ do parseAssembler "inc" `shouldParse` [InputChar]

  describe "Commands with maybe natural operand" $ do
    it "parse 'add'" $ do parseAssembler "add" `shouldParse` [Add Nothing]
    it "parse 'add 0'" $ do parseAssembler "add 0" `shouldParse` [Add (Just (Literal 0))]
    it "parse 'add 1'" $ do parseAssembler "add 1" `shouldParse` [Add (Just (Literal 1))]

    it "parse 'sub'" $ do parseAssembler "sub" `shouldParse` [Sub Nothing]
    it "parse 'sub 0'" $ do parseAssembler "sub 0" `shouldParse` [Sub (Just (Literal 0))]
    it "parse 'sub 1'" $ do parseAssembler "sub 1" `shouldParse` [Sub (Just (Literal 1))]

    it "parse 'mul'" $ do parseAssembler "mul" `shouldParse` [Mul Nothing]
    it "parse 'mul 0'" $ do parseAssembler "mul 0" `shouldParse` [Mul (Just (Literal 0))]
    it "parse 'mul 1'" $ do parseAssembler "mul 1" `shouldParse` [Mul (Just (Literal 1))]

    it "parse 'div'" $ do parseAssembler "div" `shouldParse` [Div Nothing]
    it "parse 'div 0'" $ do parseAssembler "div 0" `shouldParse` [Div (Just (Literal 0))]
    it "parse 'div 1'" $ do parseAssembler "div 1" `shouldParse` [Div (Just (Literal 1))]

    it "parse 'mod'" $ do parseAssembler "mod" `shouldParse` [Mod Nothing]
    it "parse 'mod 0'" $ do parseAssembler "mod 0" `shouldParse` [Mod (Just (Literal 0))]
    it "parse 'mod 1'" $ do parseAssembler "mod 1" `shouldParse` [Mod (Just (Literal 1))]

    it "parse 'store'" $ do parseAssembler "store" `shouldParse` [Store Nothing]
    it "parse 'store 0'" $ do parseAssembler "store 0" `shouldParse` [Store (Just (Literal 0))]
    it "parse 'store 1'" $ do parseAssembler "store 1" `shouldParse` [Store (Just (Literal 1))]

    it "parse 'retrive'" $ do parseAssembler "retrive" `shouldParse` [Load Nothing]
    it "parse 'retrive 0'" $ do parseAssembler "retrive 0" `shouldParse` [Load (Just (Literal 0))]
    it "parse 'retrive 1'" $ do parseAssembler "retrive 1" `shouldParse` [Load (Just (Literal 1))]

  describe "Commands with identifier operand" $ do
    it "parse 'label L'" $ do parseAssembler "label L" `shouldParse` [Mark "L"]
    it "parse 'call L'" $ do parseAssembler "call L" `shouldParse` [Call "L"]
    it "parse 'jump L'" $ do parseAssembler "jump L" `shouldParse` [Branch "L"]
    it "parse 'jumpz L'" $ do parseAssembler "jumpz L" `shouldParse` [BranchZ "L"]
    it "parse 'jumpn L'" $ do parseAssembler "jumpn L" `shouldParse` [BranchN "L"]
    it "parse 'jumpp L'" $ do parseAssembler "jumpp L" `shouldParse` [BranchP "L"]
    it "parse 'jumpnz L'" $ do parseAssembler "jumpnz L" `shouldParse` [BranchNZ "L"]
    it "parse 'jumppz L'" $ do parseAssembler "jumppz L" `shouldParse` [BranchPZ "L"]
    it "parse 'include L'" $ do parseAssembler "include L" `shouldParse` [Include "L"]

  describe "Commands with natural operand" $ do
    it "parse 'push 0'" $ do parseAssembler "push 0" `shouldParse` [Push (Literal 0)]
    it "parse 'push 1'" $ do parseAssembler "push 1" `shouldParse` [Push (Literal 1)]
    it "parse 'test 0'" $ do parseAssembler "test 0" `shouldParse` [Test 0]
    it "parse 'test 1'" $ do parseAssembler "test 1" `shouldParse` [Test 1]

  describe "Commands with other operand" $ do
    it "parse 'pushs \"\"'" $ do parseAssembler "pushs \"\"" `shouldParse` [PushS (Literal "")]
    it "parse 'pushs \"0\"'" $ do parseAssembler "pushs \"0\"" `shouldParse` [PushS (Literal "0")]
    it "parse 'pushs \"1\"'" $ do parseAssembler "pushs \"1\"" `shouldParse` [PushS (Literal "1")]

  describe "Comments" $ do
    it "parse ''" $ do parseAssembler "" `shouldParse` []
    it "parse ';'" $ do parseAssembler ";" `shouldParse` []
    it "parse '\n'" $ do parseAssembler ";" `shouldParse` []
    it "parse ';\n'" $ do parseAssembler ";" `shouldParse` []
    it "parse '\n;'" $ do parseAssembler ";" `shouldParse` []
    it "parse 'label L;" $ do parseAssembler "label L;" `shouldParse` [Mark "L"]
    it "parse 'label L;\n" $ do parseAssembler "label L;\n" `shouldParse` [Mark "L"]
    it "parse 'label L\n;" $ do parseAssembler "label L\n;" `shouldParse` [Mark "L"]
    it "parse 'label L\nlabel L\n" $ do parseAssembler "label L\nlabel L\n" `shouldParse` [Mark "L", Mark "L"]
    it "parse 'label L\n;label L" $ do parseAssembler "label L\n;label L" `shouldParse` [Mark "L"]
    it "parse 'label L\n;label L\n" $ do parseAssembler "label L\n;label L\n" `shouldParse` [Mark "L"]
    it "parse 'label L\nlabel L\nlabel L\n" $ do parseAssembler "label L\nlabel L\nlabel L\n" `shouldParse` [Mark "L", Mark "L", Mark "L"]

  describe "Files" $ do
    it "io"     $ do parseLibFromFile "io"     `shouldReturn` ioIL
    it "memory" $ do parseLibFromFile "memory" `shouldReturn` memoryIL
    it "prim"   $ do parseFromFile    "prim"   `shouldReturn` (Include "io" : primIL)
