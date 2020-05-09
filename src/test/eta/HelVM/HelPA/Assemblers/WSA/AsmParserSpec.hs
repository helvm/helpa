{-# LANGUAGE OverloadedStrings #-}
module HelVM.HelPA.Assemblers.WSA.AsmParserSpec (spec) where

import HelVM.HelPA.Assemblers.WSA.AsmParser
import HelVM.HelPA.Assemblers.WSA.Instruction

import HelVM.HelPA.Assemblers.WSA.TestData

import HelVM.HelPA.Common.Value


import Data.List

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Test.Hspec

spec :: Spec
spec = do
  describe "Commands without operands" $ do
    it "parse 'pop'"  $ do parseAssembler "pop" `shouldBe` Right [Pop]
    it "parse 'doub'" $ do parseAssembler "doub" `shouldBe` Right [Dup]
    it "parse 'swap'" $ do parseAssembler "swap" `shouldBe` Right [Swap]
    it "parse 'ret'"  $ do parseAssembler "ret" `shouldBe` Right [Return]
    it "parse 'exit'" $ do parseAssembler "exit" `shouldBe` Right [End]
    it "parse 'outn'" $ do parseAssembler "outn" `shouldBe` Right [OutputNum]
    it "parse 'outc'" $ do parseAssembler "outc" `shouldBe` Right [OutputChar]
    it "parse 'inn'"  $ do parseAssembler "inn" `shouldBe` Right [InputNum]
    it "parse 'inc'"  $ do parseAssembler "inc" `shouldBe` Right [InputChar]


  describe "Commands with maybe natural operand" $ do
    it "parse 'add'" $ do parseAssembler "add" `shouldBe` Right [Add Nothing]
    it "parse 'add 0'" $ do parseAssembler "add 0" `shouldBe` Right [Add (Just (Literal 0))]
    it "parse 'add 1'" $ do parseAssembler "add 1" `shouldBe` Right [Add (Just (Literal 1))]

    it "parse 'sub'" $ do parseAssembler "sub" `shouldBe` Right [Sub Nothing]
    it "parse 'sub 0'" $ do parseAssembler "sub 0" `shouldBe` Right [Sub (Just (Literal 0))]
    it "parse 'sub 1'" $ do parseAssembler "sub 1" `shouldBe` Right [Sub (Just (Literal 1))]

    it "parse 'mul'" $ do parseAssembler "mul" `shouldBe` Right [Mul Nothing]
    it "parse 'mul 0'" $ do parseAssembler "mul 0" `shouldBe` Right [Mul (Just (Literal 0))]
    it "parse 'mul 1'" $ do parseAssembler "mul 1" `shouldBe` Right [Mul (Just (Literal 1))]

    it "parse 'div'" $ do parseAssembler "div" `shouldBe` Right [Div Nothing]
    it "parse 'div 0'" $ do parseAssembler "div 0" `shouldBe` Right [Div (Just (Literal 0))]
    it "parse 'div 1'" $ do parseAssembler "div 1" `shouldBe` Right [Div (Just (Literal 1))]

    it "parse 'mod'" $ do parseAssembler "mod" `shouldBe` Right [Mod Nothing]
    it "parse 'mod 0'" $ do parseAssembler "mod 0" `shouldBe` Right [Mod (Just (Literal 0))]
    it "parse 'mod 1'" $ do parseAssembler "mod 1" `shouldBe` Right [Mod (Just (Literal 1))]

    it "parse 'store'" $ do parseAssembler "store" `shouldBe` Right [Store Nothing]
    it "parse 'store 0'" $ do parseAssembler "store 0" `shouldBe` Right [Store (Just (Literal 0))]
    it "parse 'store 1'" $ do parseAssembler "store 1" `shouldBe` Right [Store (Just (Literal 1))]

    it "parse 'retrive'" $ do parseAssembler "retrive" `shouldBe` Right [Load Nothing]
    it "parse 'retrive 0'" $ do parseAssembler "retrive 0" `shouldBe` Right [Load (Just (Literal 0))]
    it "parse 'retrive 1'" $ do parseAssembler "retrive 1" `shouldBe` Right [Load (Just (Literal 1))]

  describe "Commands with identifier operand" $ do
    it "parse 'label L'" $ do parseAssembler "label L" `shouldBe` Right [Mark "L"]
    it "parse 'call L'" $ do parseAssembler "call L" `shouldBe` Right [Call "L"]
    it "parse 'jump L'" $ do parseAssembler "jump L" `shouldBe` Right [Branch "L"]
    it "parse 'jumpz L'" $ do parseAssembler "jumpz L" `shouldBe` Right [BranchZ "L"]
    it "parse 'jumpn L'" $ do parseAssembler "jumpn L" `shouldBe` Right [BranchN "L"]
    it "parse 'jumpp L'" $ do parseAssembler "jumpp L" `shouldBe` Right [BranchP "L"]
    it "parse 'jumpnz L'" $ do parseAssembler "jumpnz L" `shouldBe` Right [BranchNZ "L"]
    it "parse 'jumppz L'" $ do parseAssembler "jumppz L" `shouldBe` Right [BranchPZ "L"]
    it "parse 'include L'" $ do parseAssembler "include L" `shouldBe` Right [Include "L"]


  describe "Commands with natural operand" $ do
    it "parse 'push 0'" $ do parseAssembler "push 0" `shouldBe` Right [Push (Literal 0)]
    it "parse 'push 1'" $ do parseAssembler "push 1" `shouldBe` Right [Push (Literal 1)]
    it "parse 'test 0'" $ do parseAssembler "test 0" `shouldBe` Right [Test 0]
    it "parse 'test 1'" $ do parseAssembler "test 1" `shouldBe` Right [Test 1]

  describe "Commands with other operand" $ do
    it "parse 'pushs \"\"'" $ do parseAssembler "pushs \"\"" `shouldBe` Right [PushS (Literal "")]
    it "parse 'pushs \"0\"'" $ do parseAssembler "pushs \"0\"" `shouldBe` Right [PushS (Literal "0")]
    it "parse 'pushs \"1\"'" $ do parseAssembler "pushs \"1\"" `shouldBe` Right [PushS (Literal "1")]

  describe "Comments" $ do
    it "parse ''" $ do parseAssembler "" `shouldBe` Right []
    it "parse ';'" $ do parseAssembler ";" `shouldBe` Right []
    it "parse '\n'" $ do parseAssembler ";" `shouldBe` Right []
    it "parse ';\n'" $ do parseAssembler ";" `shouldBe` Right []
    it "parse '\n;'" $ do parseAssembler ";" `shouldBe` Right []
    it "parse 'label L;" $ do parseAssembler "label L;" `shouldBe` Right [Mark "L"]
    it "parse 'label L;\n" $ do parseAssembler "label L;\n" `shouldBe` Right [Mark "L"]
    it "parse 'label L\n;" $ do parseAssembler "label L\n;" `shouldBe` Right [Mark "L"]
    it "parse 'label L\nlabel L\n" $ do parseAssembler "label L\nlabel L\n" `shouldBe` Right [Mark "L", Mark "L"]
    it "parse 'label L\n;label L" $ do parseAssembler "label L\n;label L" `shouldBe` Right [Mark "L"]
    it "parse 'label L\n;label L\n" $ do parseAssembler "label L\n;label L\n" `shouldBe` Right [Mark "L"]
    it "parse 'label L\nlabel L\nlabel L\n" $ do parseAssembler "label L\nlabel L\nlabel L\n" `shouldBe` Right [Mark "L", Mark "L", Mark "L"]

  describe "Files" $ do
    ioEither     <- parseLibFromFile "io"
    memoryEither <- parseLibFromFile "memory"
    primEither   <- parseFromFile    "prim"

    it "io"     $ do ioEither     `shouldBe` Right ioIL
    it "memory" $ do memoryEither `shouldBe` Right memoryIL
    it "prim"   $ do primEither   `shouldBe` Right (Include "io" : primIL)

parseLibFromFile name = parseAssembler <$> runIO (T.readFile $ "src/test/resources/wsa/libs/" ++ name ++ ".wsa")
parseFromFile    name = parseAssembler <$> runIO (T.readFile $ "src/test/resources/wsa/examples/" ++ name ++ ".wsa")
