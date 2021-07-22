module HelVM.HelPA.Assemblers.EAS.AsmParserSpec (spec) where

import           HelVM.HelPA.Assemblers.EAS.AsmParser
import           HelVM.HelPA.Assemblers.EAS.FileUtil
import           HelVM.HelPA.Assemblers.EAS.Instruction
import           HelVM.HelPA.Assemblers.EAS.TestData

import           HelVM.Expectations

import           HelVM.HelPA.Assembler.Value

import           Test.Hspec                             (Spec, describe, it)

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
      it fileName $ do parseAssembly `shouldSafeIO` il

  describe "empty" $ do
    it "parse ''" $ do parseAssemblyText "" `shouldSafe` []

  describe "Short Commands" $ do

    it "parse 'E'"  $ do parseAssemblyText "E"  `shouldSafe` [E]
    it "parse 'E '" $ do parseAssemblyText "E " `shouldSafe` [E]
    it "parse ' E'" $ do parseAssemblyText " E" `shouldSafe` [E]

    it "parse 'T'"  $ do parseAssemblyText "T"  `shouldSafe` [T]
    it "parse 'T '" $ do parseAssemblyText "T " `shouldSafe` [T]
    it "parse ' T'" $ do parseAssemblyText " T" `shouldSafe` [T]

    it "parse 'A'"  $ do parseAssemblyText "A"  `shouldSafe` [A]
    it "parse 'A '" $ do parseAssemblyText "A " `shouldSafe` [A]
    it "parse ' A'" $ do parseAssemblyText " A" `shouldSafe` [A]

    it "parse 'O'"  $ do parseAssemblyText "O"  `shouldSafe` [O]
    it "parse 'O '" $ do parseAssemblyText "O " `shouldSafe` [O]
    it "parse ' O'" $ do parseAssemblyText " O" `shouldSafe` [O]

    it "parse 'I'"  $ do parseAssemblyText "I"  `shouldSafe` [I]
    it "parse 'I '" $ do parseAssemblyText "I " `shouldSafe` [I]
    it "parse ' I'" $ do parseAssemblyText " I" `shouldSafe` [I]

    it "parse 'S'"  $ do parseAssemblyText "S"  `shouldSafe` [S]
    it "parse 'S '" $ do parseAssemblyText "S " `shouldSafe` [S]
    it "parse ' S'" $ do parseAssemblyText " S" `shouldSafe` [S]

    it "parse 'H'"  $ do parseAssemblyText "H"  `shouldSafe` [H]
    it "parse 'H '" $ do parseAssemblyText "H " `shouldSafe` [H]
    it "parse ' H'" $ do parseAssemblyText " H" `shouldSafe` [H]

  describe "Short Commands - Numbers" $ do

    it "parse 'N0'"   $ do parseAssemblyText "N0"    `shouldSafe` [N (Literal 0)]
    it "parse 'N0 '"  $ do parseAssemblyText "N0 "   `shouldSafe` [N (Literal 0)]
    it "parse ' N0'"  $ do parseAssemblyText " N0"   `shouldSafe` [N (Literal 0)]

    it "parse 'N00'"  $ do parseAssemblyText "N00"   `shouldSafe` [N (Literal 0)]
    it "parse 'N00 '" $ do parseAssemblyText "N00 "  `shouldSafe` [N (Literal 0)]
    it "parse ' N00'" $ do parseAssemblyText " N00"  `shouldSafe` [N (Literal 0)]

    it "parse 'N1'"   $ do parseAssemblyText "N1"    `shouldSafe` [N (Literal 1)]
    it "parse 'N1 '"  $ do parseAssemblyText "N1 "   `shouldSafe` [N (Literal 1)]
    it "parse ' N1'"  $ do parseAssemblyText " N1"   `shouldSafe` [N (Literal 1)]

    it "parse 'N01'"  $ do parseAssemblyText "N01"   `shouldSafe` [N (Literal 1)]
    it "parse 'N01 '" $ do parseAssemblyText "N01 "  `shouldSafe` [N (Literal 1)]
    it "parse ' N01'" $ do parseAssemblyText " N01"  `shouldSafe` [N (Literal 1)]

    it "parse 'N10'"  $ do parseAssemblyText "N10"   `shouldSafe` [N (Literal 10)]
    it "parse 'N10 '" $ do parseAssemblyText "N10 "  `shouldSafe` [N (Literal 10)]
    it "parse ' N10'" $ do parseAssemblyText " N10"  `shouldSafe` [N (Literal 10)]

    it "parse 'N' ''" $ do parseAssemblyText "N' "  `shouldSafe` [N (Literal 32)]
    it "parse 'N''''" $ do parseAssemblyText "N''" `shouldSafe` [N (Literal 39)]
    it "parse 'N'0''" $ do parseAssemblyText "N'0"  `shouldSafe` [N (Literal 48)]
    it "parse 'N'N''" $ do parseAssemblyText "N'N"  `shouldSafe` [N (Literal 78)]

    it "parse 'N<label '" $ do parseAssemblyText "N<label " `shouldSafe` [N (Variable "label")]
    it "parse ' N<label'" $ do parseAssemblyText " N<label" `shouldSafe` [N (Variable "label")]

  describe "Long Commends" $ do
    it "parse 'dividE'"    $ do parseAssemblyText "dividE"    `shouldSafe` [E]
    it "parse 'Transfer'"  $ do parseAssemblyText "Transfer"  `shouldSafe` [T]
    it "parse 'Address'"   $ do parseAssemblyText "Address"   `shouldSafe` [A]
    it "parse 'Output'"    $ do parseAssemblyText "Output"    `shouldSafe` [O]
    it "parse 'Input'"     $ do parseAssemblyText "Input"     `shouldSafe` [I]
    it "parse 'Substract'" $ do parseAssemblyText "Substract" `shouldSafe` [S]
    it "parse 'Halibut'"   $ do parseAssemblyText "Halibut"   `shouldSafe` [H]

  describe "Long Commends - Numbers" $ do
    it "parse 'Number 0'"   $ do parseAssemblyText "Number 0"  `shouldSafe` [N (Literal 0)]
    it "parse 'Number 00'"  $ do parseAssemblyText "Number 00" `shouldSafe` [N (Literal 0)]
    it "parse 'Number 1'"   $ do parseAssemblyText "Number 1"  `shouldSafe` [N (Literal 1)]
    it "parse 'Number 01'"  $ do parseAssemblyText "Number 01" `shouldSafe` [N (Literal 1)]
    it "parse 'Number 10'"  $ do parseAssemblyText "Number 10" `shouldSafe` [N (Literal 10)]
    it "parse 'Number ' ''" $ do parseAssemblyText "Number ' " `shouldSafe` [N (Literal 32)]
    it "parse 'Number ''''" $ do parseAssemblyText "Number ''" `shouldSafe` [N (Literal 39)]
    it "parse 'Number '0''" $ do parseAssemblyText "Number '0" `shouldSafe` [N (Literal 48)]
    it "parse 'Number 'N''" $ do parseAssemblyText "Number 'N" `shouldSafe` [N (Literal 78)]
    it "parse 'Number <label'" $ do parseAssemblyText "Number <label" `shouldSafe` [N (Variable "label")]

  describe "Labels" $ do

    it "parse '*label\n'"   $ do parseAssemblyText "*label\n"   `shouldSafe` [D "label"]
    it "parse '*label\n '"  $ do parseAssemblyText "*label\n "  `shouldSafe` [D "label"]
    it "parse ' *label\n'"  $ do parseAssemblyText " *label\n"  `shouldSafe` [D "label"]

    it "parse '>label:'"    $ do parseAssemblyText ">label:"   `shouldSafe` [L "label"]
    it "parse '>label: '"   $ do parseAssemblyText ">label: "   `shouldSafe` [L "label"]
    it "parse ' >label:'"   $ do parseAssemblyText " >label:"  ` shouldSafe` [L "label"]

    it "parse '\"label\"'"  $ do parseAssemblyText "\"label\""  `shouldSafe` [U "label"]
    it "parse '\"label\" '" $ do parseAssemblyText "\"label\" " `shouldSafe` [U "label"]
    it "parse ' \"label\"'" $ do parseAssemblyText " \"label\"" `shouldSafe` [U "label"]

    it "parse '>LOOP: Input'"   $ do parseAssemblyText ">LOOP: Input"   `shouldSafe` [L "LOOP",I]
    it "parse '>WRITE: Output'" $ do parseAssemblyText ">WRITE: Output" `shouldSafe` [L "WRITE",O]

  describe "Return" $ do
    it "parse '\n'"  $ do parseAssemblyText "\n"  `shouldSafe` [R]
    it "parse '\n '" $ do parseAssemblyText "\n " `shouldSafe` [R]
    it "parse ' \n'" $ do parseAssemblyText " \n" `shouldSafe` [R]

    it "parse 'A N0 T \nA N0 T \n'" $ do parseAssemblyText "A N0 T \nA N0 T \n" `shouldSafe` [A,N (Literal 0),T,R,A,N (Literal 0),T,R]

  describe "Comments" $ do
    it "parse ' A N0 T \n '"  $ do parseAssemblyText " A N0 T \n "  `shouldSafe` [A , N (Literal 0), T , R]
    it "parse '# A N0 T \n '" $ do parseAssemblyText "# A N0 T \n " `shouldSafe` []
    it "parse ' #A N0 T \n '" $ do parseAssemblyText " #A N0 T \n " `shouldSafe` [R]
    it "parse ' A# N0 T \n '" $ do parseAssemblyText " A# N0 T \n " `shouldSafe` [A , R]
    it "parse ' A #N0 T \n '" $ do parseAssemblyText " A #N0 T \n " `shouldSafe` [A , R]
