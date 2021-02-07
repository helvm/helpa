module HelVM.HelPA.Assemblers.EAS.AsmParserSpec (spec) where

import HelVM.HelPA.Assemblers.EAS.AsmParser
import HelVM.HelPA.Assemblers.EAS.Instruction
import HelVM.HelPA.Assemblers.EAS.FileUtil
import HelVM.HelPA.Assemblers.EAS.TestData

import HelVM.HelPA.Assemblers.Expectations

import HelVM.HelPA.Common.Value

import Test.Hspec
import Test.Hspec.Attoparsec

spec :: Spec
spec = do
  describe "parseAssemblyText" $ do
    forM_ [ ("true"     , trueIL)
          , ("hello"    , helloIL)
          , ("pip"      , pipIL)
          , ("pip2"     , pip2IL)
          , ("reverse"  , reverseIL)
          , ("function" , functionIL)
          , ("writestr" , writeStrIL)
          , ("hello2"   , hello2IL <> [D "writestr.eas"])
          , ("hello3"   , hello2IL <> [D "writestr.eas"])
          , ("hello4"   , hello4IL <> [D "writestr.eas"])
          , ("writenum" , writeNumIL)
          , ("multiply" , multiplyIL)
          , ("readnum"  , readNumIL)
          , ("fact"     , factIL   <> [D "readnum.eas",D "writenum.eas",D "multiply.eas",D "writestr.eas"])
          , ("bottles"  , bottlesIL)
          , ("euclid"   , euclidIL)
          ] $ \(fileName , il) -> do
      let parseAssembly = parseAssemblyText <$> readFileText (buildAbsolutePathToEasFile fileName)
      it fileName $ do parseAssembly `shouldParseReturn` il

  describe "empty" $ do
    it "parse ''" $ do parseAssemblyText "" `shouldParse` []

  describe "Short Commands" $ do

    it "parse 'E'"  $ do parseAssemblyText "E"  `shouldParse` [E]
    it "parse 'E '" $ do parseAssemblyText "E " `shouldParse` [E]
    it "parse ' E'" $ do parseAssemblyText " E" `shouldParse` [E]

    it "parse 'T'"  $ do parseAssemblyText "T"  `shouldParse` [T]
    it "parse 'T '" $ do parseAssemblyText "T " `shouldParse` [T]
    it "parse ' T'" $ do parseAssemblyText " T" `shouldParse` [T]

    it "parse 'A'"  $ do parseAssemblyText "A"  `shouldParse` [A]
    it "parse 'A '" $ do parseAssemblyText "A " `shouldParse` [A]
    it "parse ' A'" $ do parseAssemblyText " A" `shouldParse` [A]

    it "parse 'O'"  $ do parseAssemblyText "O"  `shouldParse` [O]
    it "parse 'O '" $ do parseAssemblyText "O " `shouldParse` [O]
    it "parse ' O'" $ do parseAssemblyText " O" `shouldParse` [O]

    it "parse 'I'"  $ do parseAssemblyText "I"  `shouldParse` [I]
    it "parse 'I '" $ do parseAssemblyText "I " `shouldParse` [I]
    it "parse ' I'" $ do parseAssemblyText " I" `shouldParse` [I]

    it "parse 'S'"  $ do parseAssemblyText "S"  `shouldParse` [S]
    it "parse 'S '" $ do parseAssemblyText "S " `shouldParse` [S]
    it "parse ' S'" $ do parseAssemblyText " S" `shouldParse` [S]

    it "parse 'H'"  $ do parseAssemblyText "H"  `shouldParse` [H]
    it "parse 'H '" $ do parseAssemblyText "H " `shouldParse` [H]
    it "parse ' H'" $ do parseAssemblyText " H" `shouldParse` [H]

  describe "Short Commands - Numbers" $ do

    it "parse 'N0'"   $ do parseAssemblyText "N0"    `shouldParse` [N (Literal 0)]
    it "parse 'N0 '"  $ do parseAssemblyText "N0 "   `shouldParse` [N (Literal 0)]
    it "parse ' N0'"  $ do parseAssemblyText " N0"   `shouldParse` [N (Literal 0)]

    it "parse 'N00'"  $ do parseAssemblyText "N00"   `shouldParse` [N (Literal 0)]
    it "parse 'N00 '" $ do parseAssemblyText "N00 "  `shouldParse` [N (Literal 0)]
    it "parse ' N00'" $ do parseAssemblyText " N00"  `shouldParse` [N (Literal 0)]

    it "parse 'N1'"   $ do parseAssemblyText "N1"    `shouldParse` [N (Literal 1)]
    it "parse 'N1 '"  $ do parseAssemblyText "N1 "   `shouldParse` [N (Literal 1)]
    it "parse ' N1'"  $ do parseAssemblyText " N1"   `shouldParse` [N (Literal 1)]

    it "parse 'N01'"  $ do parseAssemblyText "N01"   `shouldParse` [N (Literal 1)]
    it "parse 'N01 '" $ do parseAssemblyText "N01 "  `shouldParse` [N (Literal 1)]
    it "parse ' N01'" $ do parseAssemblyText " N01"  `shouldParse` [N (Literal 1)]

    it "parse 'N10'"  $ do parseAssemblyText "N10"   `shouldParse` [N (Literal 10)]
    it "parse 'N10 '" $ do parseAssemblyText "N10 "  `shouldParse` [N (Literal 10)]
    it "parse ' N10'" $ do parseAssemblyText " N10"  `shouldParse` [N (Literal 10)]

    it "parse 'N' ''" $ do parseAssemblyText "N' '"  `shouldParse` [N (Literal 32)]
    it "parse 'N''''" $ do parseAssemblyText "N'\''" `shouldParse` [N (Literal 39)]
    it "parse 'N'0''" $ do parseAssemblyText "N'0'"  `shouldParse` [N (Literal 48)]
    it "parse 'N'N''" $ do parseAssemblyText "N'N'"  `shouldParse` [N (Literal 78)]

    it "parse 'N<label '" $ do parseAssemblyText "N<label " `shouldParse` [N (Variable "label")]
    it "parse ' N<label'" $ do parseAssemblyText " N<label" `shouldParse` [N (Variable "label")]

  describe "Long Commends" $ do
    it "parse 'dividE'"    $ do parseAssemblyText "dividE"    `shouldParse` [E]
    it "parse 'Transfer'"  $ do parseAssemblyText "Transfer"  `shouldParse` [T]
    it "parse 'Address'"   $ do parseAssemblyText "Address"   `shouldParse` [A]
    it "parse 'Output'"    $ do parseAssemblyText "Output"    `shouldParse` [O]
    it "parse 'Input'"     $ do parseAssemblyText "Input"     `shouldParse` [I]
    it "parse 'Substract'" $ do parseAssemblyText "Substract" `shouldParse` [S]
    it "parse 'Halibut'"   $ do parseAssemblyText "Halibut"   `shouldParse` [H]

  describe "Long Commends - Numbers" $ do
    it "parse 'Number 0'"   $ do parseAssemblyText "Number 0"    `shouldParse` [N (Literal 0)]
    it "parse 'Number 00'"  $ do parseAssemblyText "Number 00"   `shouldParse` [N (Literal 0)]
    it "parse 'Number 1'"   $ do parseAssemblyText "Number 1"    `shouldParse` [N (Literal 1)]
    it "parse 'Number 01'"  $ do parseAssemblyText "Number 01"   `shouldParse` [N (Literal 1)]
    it "parse 'Number 10'"  $ do parseAssemblyText "Number 10"   `shouldParse` [N (Literal 10)]
    it "parse 'Number ' ''" $ do parseAssemblyText "Number ' '"  `shouldParse` [N (Literal 32)]
    it "parse 'Number ''''" $ do parseAssemblyText "Number '\''" `shouldParse` [N (Literal 39)]
    it "parse 'Number '0''" $ do parseAssemblyText "Number '0'"  `shouldParse` [N (Literal 48)]
    it "parse 'Number 'N''" $ do parseAssemblyText "Number 'N'"  `shouldParse` [N (Literal 78)]
    it "parse 'Number <label'" $ do parseAssemblyText "Number <label" `shouldParse` [N (Variable "label")]

  describe "Labels" $ do

    it "parse '*label\n'"   $ do parseAssemblyText "*label\n"   `shouldParse` [D "label"]
    it "parse '*label\n '"  $ do parseAssemblyText "*label\n "  `shouldParse` [D "label"]
    it "parse ' *label\n'"  $ do parseAssemblyText " *label\n"  `shouldParse` [D "label"]

    it "parse '>label:'"    $ do parseAssemblyText ">label:"   `shouldParse` [L "label"]
    it "parse '>label: '"   $ do parseAssemblyText ">label: "   `shouldParse` [L "label"]
    it "parse ' >label:'"   $ do parseAssemblyText " >label:"  ` shouldParse` [L "label"]

    it "parse '\"label\"'"  $ do parseAssemblyText "\"label\""  `shouldParse` [U "label"]
    it "parse '\"label\" '" $ do parseAssemblyText "\"label\" " `shouldParse` [U "label"]
    it "parse ' \"label\"'" $ do parseAssemblyText " \"label\"" `shouldParse` [U "label"]

    it "parse '>LOOP: Input'"   $ do parseAssemblyText ">LOOP: Input"   `shouldParse` [L "LOOP",I]
    it "parse '>WRITE: Output'" $ do parseAssemblyText ">WRITE: Output" `shouldParse` [L "WRITE",O]

  describe "Return" $ do
    it "parse '\n'"  $ do parseAssemblyText "\n"  `shouldParse` [R]
    it "parse '\n '" $ do parseAssemblyText "\n " `shouldParse` [R]
    it "parse ' \n'" $ do parseAssemblyText " \n" `shouldParse` [R]

    it "parse 'A N0 T \nA N0 T \n'" $ do parseAssemblyText "A N0 T \nA N0 T \n" `shouldParse` [A,N (Literal 0),T,R,A,N (Literal 0),T,R]

  describe "Comments" $ do
    it "parse ' A N0 T \n '"  $ do parseAssemblyText " A N0 T \n "  `shouldParse` [A, N (Literal 0), T, R]
    it "parse '# A N0 T \n '" $ do parseAssemblyText "# A N0 T \n " `shouldParse` []
    it "parse ' #A N0 T \n '" $ do parseAssemblyText " #A N0 T \n " `shouldParse` [R]
    it "parse ' A# N0 T \n '" $ do parseAssemblyText " A# N0 T \n " `shouldParse` [A, R]
    it "parse ' A #N0 T \n '" $ do parseAssemblyText " A #N0 T \n " `shouldParse` [A, R]
