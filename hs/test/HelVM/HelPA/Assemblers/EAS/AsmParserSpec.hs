module HelVM.HelPA.Assemblers.EAS.AsmParserSpec (spec) where

import HelVM.HelPA.Assemblers.EAS.AsmParser
import HelVM.HelPA.Assemblers.EAS.Instruction

import HelVM.HelPA.Assemblers.EAS.TestData
import HelVM.HelPA.Assemblers.EAS.FileUtil
import HelVM.HelPA.Assemblers.Util

import HelVM.HelPA.Common.Value

import Data.List

import Test.Hspec
import Test.Hspec.Attoparsec

spec :: Spec
spec = do
  describe "empty" $ do
    it "parse ''" $ do parseAssembler "" `shouldParse` []

  describe "Short Commands" $ do

    it "parse 'E'" $ do parseAssembler "E" `shouldParse` [E]
    it "parse 'E '" $ do parseAssembler "E " `shouldParse` [E]
    it "parse ' E'" $ do parseAssembler " E" `shouldParse` [E]

    it "parse 'T'" $ do parseAssembler "T" `shouldParse` [T]
    it "parse 'T '" $ do parseAssembler "T " `shouldParse` [T]
    it "parse ' T'" $ do parseAssembler " T" `shouldParse` [T]

    it "parse 'A'" $ do parseAssembler "A" `shouldParse` [A]
    it "parse 'A '" $ do parseAssembler "A " `shouldParse` [A]
    it "parse ' A'" $ do parseAssembler " A" `shouldParse` [A]

    it "parse 'O'" $ do parseAssembler "O" `shouldParse` [O]
    it "parse 'O '" $ do parseAssembler "O " `shouldParse` [O]
    it "parse ' O'" $ do parseAssembler " O" `shouldParse` [O]

    it "parse 'I'" $ do parseAssembler "I" `shouldParse` [I]
    it "parse 'I '" $ do parseAssembler "I " `shouldParse` [I]
    it "parse ' I'" $ do parseAssembler " I" `shouldParse` [I]

    it "parse 'S'" $ do parseAssembler "S" `shouldParse` [S]
    it "parse 'S '" $ do parseAssembler "S " `shouldParse` [S]
    it "parse ' S'" $ do parseAssembler " S" `shouldParse` [S]

    it "parse 'H'" $ do parseAssembler "H" `shouldParse` [H]
    it "parse 'H '" $ do parseAssembler "H " `shouldParse` [H]
    it "parse ' H'" $ do parseAssembler " H" `shouldParse` [H]

  describe "Short Commands - Numbers" $ do

    it "parse 'N0'" $ do parseAssembler "N0" `shouldParse` [N (Literal 0)]
    it "parse 'N0 '" $ do parseAssembler "N0 " `shouldParse` [N (Literal 0)]
    it "parse ' N0'" $ do parseAssembler " N0" `shouldParse` [N (Literal 0)]

    it "parse 'N00'" $ do parseAssembler "N00" `shouldParse` [N (Literal 0)]
    it "parse 'N00 '" $ do parseAssembler "N00 " `shouldParse` [N (Literal 0)]
    it "parse ' N00'" $ do parseAssembler " N00" `shouldParse` [N (Literal 0)]

    it "parse 'N1'" $ do parseAssembler "N1" `shouldParse` [N (Literal 1)]
    it "parse 'N1 '" $ do parseAssembler "N1 " `shouldParse` [N (Literal 1)]
    it "parse ' N1'" $ do parseAssembler " N1" `shouldParse` [N (Literal 1)]

    it "parse 'N01'" $ do parseAssembler "N01" `shouldParse` [N (Literal 1)]
    it "parse 'N01 '" $ do parseAssembler "N01 " `shouldParse` [N (Literal 1)]
    it "parse ' N01'" $ do parseAssembler " N01" `shouldParse` [N (Literal 1)]

    it "parse 'N10'" $ do parseAssembler "N10" `shouldParse` [N (Literal 10)]
    it "parse 'N10 '" $ do parseAssembler "N10 " `shouldParse` [N (Literal 10)]
    it "parse ' N10'" $ do parseAssembler " N10" `shouldParse` [N (Literal 10)]

    it "parse 'N' ''" $ do parseAssembler "N' '" `shouldParse` [N (Literal 32)]
    it "parse 'N''''" $ do parseAssembler "N'\''" `shouldParse` [N (Literal 39)]
    it "parse 'N'0''" $ do parseAssembler "N'0'" `shouldParse` [N (Literal 48)]
    it "parse 'N'N''" $ do parseAssembler "N'N'" `shouldParse` [N (Literal 78)]

    it "parse 'N<label '" $ do parseAssembler "N<label " `shouldParse` [N (Variable "label")]
    it "parse ' N<label'" $ do parseAssembler " N<label" `shouldParse` [N (Variable "label")]

  describe "Long Commends" $ do

    it "parse 'dividE'" $ do parseAssembler "dividE" `shouldParse` [E]
    it "parse 'Transfer'" $ do parseAssembler "Transfer" `shouldParse` [T]
    it "parse 'Address'" $ do parseAssembler "Address" `shouldParse` [A]
    it "parse 'Output'" $ do parseAssembler "Output" `shouldParse` [O]
    it "parse 'Input'" $ do parseAssembler "Input" `shouldParse` [I]
    it "parse 'Substract'" $ do parseAssembler "Substract" `shouldParse` [S]
    it "parse 'Halibut'" $ do parseAssembler "Halibut" `shouldParse` [H]

  describe "Long Commends - Numbers" $ do

    it "parse 'Number 0'" $ do parseAssembler "Number 0" `shouldParse` [N (Literal 0)]
    it "parse 'Number 00'" $ do parseAssembler "Number 00" `shouldParse` [N (Literal 0)]
    it "parse 'Number 1'" $ do parseAssembler "Number 1" `shouldParse` [N (Literal 1)]
    it "parse 'Number 01'" $ do parseAssembler "Number 01" `shouldParse` [N (Literal 1)]
    it "parse 'Number 10'" $ do parseAssembler "Number 10" `shouldParse` [N (Literal 10)]
    it "parse 'Number ' ''" $ do parseAssembler "Number ' '" `shouldParse` [N (Literal 32)]
    it "parse 'Number ''''" $ do parseAssembler "Number '\''" `shouldParse` [N (Literal 39)]
    it "parse 'Number '0''" $ do parseAssembler "Number '0'" `shouldParse` [N (Literal 48)]
    it "parse 'Number 'N''" $ do parseAssembler "Number 'N'" `shouldParse` [N (Literal 78)]
    it "parse 'Number <label'" $ do parseAssembler "Number <label" `shouldParse` [N (Variable "label")]

  describe "Labels" $ do

    it "parse '*label\n'" $ do parseAssembler "*label\n" `shouldParse` [D "label"]
    it "parse '*label\n '" $ do parseAssembler "*label\n " `shouldParse` [D "label"]
    it "parse ' *label\n'" $ do parseAssembler " *label\n" `shouldParse` [D "label"]

    it "parse '>label:'" $ do parseAssembler ">label:" `shouldParse` [L "label"]
    it "parse '>label: '" $ do parseAssembler ">label: " `shouldParse` [L "label"]
    it "parse ' >label:'" $ do parseAssembler " >label:" `shouldParse` [L "label"]

    it "parse '\"label\"'" $ do parseAssembler "\"label\"" `shouldParse` [U "label"]
    it "parse '\"label\" '" $ do parseAssembler "\"label\" " `shouldParse` [U "label"]
    it "parse ' \"label\"'" $ do parseAssembler " \"label\"" `shouldParse` [U "label"]

    it "parse '>LOOP: Input'" $ do parseAssembler ">LOOP: Input" `shouldParse` [L "LOOP",I]
    it "parse '>WRITE: Output'" $ do parseAssembler ">WRITE: Output" `shouldParse` [L "WRITE",O]

  describe "Return" $ do

    it "parse '\n'" $ do parseAssembler "\n" `shouldParse` [R]
    it "parse '\n '" $ do parseAssembler "\n " `shouldParse` [R]
    it "parse ' \n'" $ do parseAssembler " \n" `shouldParse` [R]

    it "parse 'A N0 T \nA N0 T \n'" $ do parseAssembler "A N0 T \nA N0 T \n" `shouldParse` [A,N (Literal 0),T,R,A,N (Literal 0),T,R]

  describe "Comments" $ do

    it "parse ' A N0 T \n '" $ do parseAssembler " A N0 T \n " `shouldParse` [A, N (Literal 0), T, R]
    it "parse '# A N0 T \n '" $ do parseAssembler "# A N0 T \n " `shouldParse` []
    it "parse ' #A N0 T \n '" $ do parseAssembler " #A N0 T \n " `shouldParse` [R]
    it "parse ' A# N0 T \n '" $ do parseAssembler " A# N0 T \n " `shouldParse` [A, R]
    it "parse ' A #N0 T \n '" $ do parseAssembler " A #N0 T \n " `shouldParse` [A, R]

  describe "Files" $ do
    it "true"     $ do parseFromFile "true"     `shouldReturn` trueIL
    it "hello"    $ do parseFromFile "hello"    `shouldReturn` helloIL
    it "pip"      $ do parseFromFile "pip"      `shouldReturn` pipIL
    it "pip2"     $ do parseFromFile "pip2"     `shouldReturn` pip2IL
    it "reverse"  $ do parseFromFile "reverse"  `shouldReturn` reverseIL
    it "function" $ do parseFromFile "function" `shouldReturn` functionIL
    it "writestr" $ do parseFromFile "writestr" `shouldReturn` writeStrIL
    it "hello2"   $ do parseFromFile "hello2"   `shouldReturn` (hello2IL <> [D "writestr.eas"])
    it "hello3"   $ do parseFromFile "hello3"   `shouldReturn` (hello2IL <> [D "writestr.eas"])
    it "hello4"   $ do parseFromFile "hello4"   `shouldReturn` (hello4IL <> [D "writestr.eas"])
    it "writenum" $ do parseFromFile "writenum" `shouldReturn` writeNumIL
    it "multiply" $ do parseFromFile "multiply" `shouldReturn` multiplyIL
    it "readnum"  $ do parseFromFile "readnum"  `shouldReturn` readNumIL
    it "fact"     $ do parseFromFile "fact"     `shouldReturn` (factIL   <> [D "readnum.eas",D "writenum.eas",D "multiply.eas",D "writestr.eas"])
    it "bottles"  $ do parseFromFile "bottles"  `shouldReturn` bottlesIL
    it "euclid"   $ do parseFromFile "euclid"   `shouldReturn` euclidIL
