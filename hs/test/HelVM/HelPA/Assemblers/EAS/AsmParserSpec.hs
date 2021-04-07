module HelVM.HelPA.Assemblers.EAS.AsmParserSpec (spec) where

import HelVM.HelPA.Assemblers.EAS.AsmParser
import HelVM.HelPA.Assemblers.EAS.Instruction

import HelVM.HelPA.Assemblers.EAS.TestData

import HelVM.HelPA.Common.Value

import Data.List

import Test.Hspec

spec :: Spec
spec = do
  describe "empty" $ do
    it "parse ''" $ do parseAssembler "" `shouldBe` Right []

  describe "Short Commands" $ do

    it "parse 'E'" $ do parseAssembler "E" `shouldBe` Right [E]
    it "parse 'E '" $ do parseAssembler "E " `shouldBe` Right [E]
    it "parse ' E'" $ do parseAssembler " E" `shouldBe` Right [E]

    it "parse 'T'" $ do parseAssembler "T" `shouldBe` Right [T]
    it "parse 'T '" $ do parseAssembler "T " `shouldBe` Right [T]
    it "parse ' T'" $ do parseAssembler " T" `shouldBe` Right [T]

    it "parse 'A'" $ do parseAssembler "A" `shouldBe` Right [A]
    it "parse 'A '" $ do parseAssembler "A " `shouldBe` Right [A]
    it "parse ' A'" $ do parseAssembler " A" `shouldBe` Right [A]

    it "parse 'O'" $ do parseAssembler "O" `shouldBe` Right [O]
    it "parse 'O '" $ do parseAssembler "O " `shouldBe` Right [O]
    it "parse ' O'" $ do parseAssembler " O" `shouldBe` Right [O]

    it "parse 'I'" $ do parseAssembler "I" `shouldBe` Right [I]
    it "parse 'I '" $ do parseAssembler "I " `shouldBe` Right [I]
    it "parse ' I'" $ do parseAssembler " I" `shouldBe` Right [I]

    it "parse 'S'" $ do parseAssembler "S" `shouldBe` Right [S]
    it "parse 'S '" $ do parseAssembler "S " `shouldBe` Right [S]
    it "parse ' S'" $ do parseAssembler " S" `shouldBe` Right [S]

    it "parse 'H'" $ do parseAssembler "H" `shouldBe` Right [H]
    it "parse 'H '" $ do parseAssembler "H " `shouldBe` Right [H]
    it "parse ' H'" $ do parseAssembler " H" `shouldBe` Right [H]

  describe "Short Commands - Numbers" $ do

    it "parse 'N0'" $ do parseAssembler "N0" `shouldBe` Right [N (Literal 0)]
    it "parse 'N0 '" $ do parseAssembler "N0 " `shouldBe` Right [N (Literal 0)]
    it "parse ' N0'" $ do parseAssembler " N0" `shouldBe` Right [N (Literal 0)]

    it "parse 'N00'" $ do parseAssembler "N00" `shouldBe` Right [N (Literal 0)]
    it "parse 'N00 '" $ do parseAssembler "N00 " `shouldBe` Right [N (Literal 0)]
    it "parse ' N00'" $ do parseAssembler " N00" `shouldBe` Right [N (Literal 0)]

    it "parse 'N1'" $ do parseAssembler "N1" `shouldBe` Right [N (Literal 1)]
    it "parse 'N1 '" $ do parseAssembler "N1 " `shouldBe` Right [N (Literal 1)]
    it "parse ' N1'" $ do parseAssembler " N1" `shouldBe` Right [N (Literal 1)]

    it "parse 'N01'" $ do parseAssembler "N01" `shouldBe` Right [N (Literal 1)]
    it "parse 'N01 '" $ do parseAssembler "N01 " `shouldBe` Right [N (Literal 1)]
    it "parse ' N01'" $ do parseAssembler " N01" `shouldBe` Right [N (Literal 1)]

    it "parse 'N10'" $ do parseAssembler "N10" `shouldBe` Right [N (Literal 10)]
    it "parse 'N10 '" $ do parseAssembler "N10 " `shouldBe` Right [N (Literal 10)]
    it "parse ' N10'" $ do parseAssembler " N10" `shouldBe` Right [N (Literal 10)]

    it "parse 'N' ''" $ do parseAssembler "N' '" `shouldBe` Right [N (Literal 32)]
    it "parse 'N''''" $ do parseAssembler "N'\''" `shouldBe` Right [N (Literal 39)]
    it "parse 'N'0''" $ do parseAssembler "N'0'" `shouldBe` Right [N (Literal 48)]
    it "parse 'N'N''" $ do parseAssembler "N'N'" `shouldBe` Right [N (Literal 78)]

    it "parse 'N<label '" $ do parseAssembler "N<label " `shouldBe` Right [N (Variable "label")]
    it "parse ' N<label'" $ do parseAssembler " N<label" `shouldBe` Right [N (Variable "label")]

  describe "Long Commends" $ do

    it "parse 'dividE'" $ do parseAssembler "dividE" `shouldBe` Right [E]
    it "parse 'Transfer'" $ do parseAssembler "Transfer" `shouldBe` Right [T]
    it "parse 'Address'" $ do parseAssembler "Address" `shouldBe` Right [A]
    it "parse 'Output'" $ do parseAssembler "Output" `shouldBe` Right [O]
    it "parse 'Input'" $ do parseAssembler "Input" `shouldBe` Right [I]
    it "parse 'Substract'" $ do parseAssembler "Substract" `shouldBe` Right [S]
    it "parse 'Halibut'" $ do parseAssembler "Halibut" `shouldBe` Right [H]

  describe "Long Commends - Numbers" $ do

    it "parse 'Number 0'" $ do parseAssembler "Number 0" `shouldBe` Right [N (Literal 0)]
    it "parse 'Number 00'" $ do parseAssembler "Number 00" `shouldBe` Right [N (Literal 0)]
    it "parse 'Number 1'" $ do parseAssembler "Number 1" `shouldBe` Right [N (Literal 1)]
    it "parse 'Number 01'" $ do parseAssembler "Number 01" `shouldBe` Right [N (Literal 1)]
    it "parse 'Number 10'" $ do parseAssembler "Number 10" `shouldBe` Right [N (Literal 10)]
    it "parse 'Number ' ''" $ do parseAssembler "Number ' '" `shouldBe` Right [N (Literal 32)]
    it "parse 'Number ''''" $ do parseAssembler "Number '\''" `shouldBe` Right [N (Literal 39)]
    it "parse 'Number '0''" $ do parseAssembler "Number '0'" `shouldBe` Right [N (Literal 48)]
    it "parse 'Number 'N''" $ do parseAssembler "Number 'N'" `shouldBe` Right [N (Literal 78)]
    it "parse 'Number <label'" $ do parseAssembler "Number <label" `shouldBe` Right [N (Variable "label")]

  describe "Labels" $ do

    it "parse '*label\n'" $ do parseAssembler "*label\n" `shouldBe` Right [D "label"]
    it "parse '*label\n '" $ do parseAssembler "*label\n " `shouldBe` Right [D "label"]
    it "parse ' *label\n'" $ do parseAssembler " *label\n" `shouldBe` Right [D "label"]

    it "parse '>label:'" $ do parseAssembler ">label:" `shouldBe` Right [L "label"]
    it "parse '>label: '" $ do parseAssembler ">label: " `shouldBe` Right [L "label"]
    it "parse ' >label:'" $ do parseAssembler " >label:" `shouldBe` Right [L "label"]

    it "parse '\"label\"'" $ do parseAssembler "\"label\"" `shouldBe` Right [U "label"]
    it "parse '\"label\" '" $ do parseAssembler "\"label\" " `shouldBe` Right [U "label"]
    it "parse ' \"label\"'" $ do parseAssembler " \"label\"" `shouldBe` Right [U "label"]

    it "parse '>LOOP: Input'" $ do parseAssembler ">LOOP: Input" `shouldBe` Right [L "LOOP",I]
    it "parse '>WRITE: Output'" $ do parseAssembler ">WRITE: Output" `shouldBe` Right [L "WRITE",O]

  describe "Return" $ do

    it "parse '\n'" $ do parseAssembler "\n" `shouldBe` Right [R]
    it "parse '\n '" $ do parseAssembler "\n " `shouldBe` Right [R]
    it "parse ' \n'" $ do parseAssembler " \n" `shouldBe` Right [R]

    it "parse 'A N0 T \nA N0 T \n'" $ do parseAssembler "A N0 T \nA N0 T \n" `shouldBe` Right [A,N (Literal 0),T,R,A,N (Literal 0),T,R]

  describe "Comments" $ do

    it "parse ' A N0 T \n '" $ do parseAssembler " A N0 T \n " `shouldBe` Right [A, N (Literal 0), T, R]
    it "parse '# A N0 T \n '" $ do parseAssembler "# A N0 T \n " `shouldBe` Right []
    it "parse ' #A N0 T \n '" $ do parseAssembler " #A N0 T \n " `shouldBe` Right [R]
    it "parse ' A# N0 T \n '" $ do parseAssembler " A# N0 T \n " `shouldBe` Right [A, R]
    it "parse ' A #N0 T \n '" $ do parseAssembler " A #N0 T \n " `shouldBe` Right [A, R]

  describe "Files" $ do
    trueEither     <- parseFromFile "true"
    helloEither    <- parseFromFile "hello"
    pipEither      <- parseFromFile "pip"
    pip2Either     <- parseFromFile "pip2"
    reverseEither  <- parseFromFile "reverse"
    functionEither <- parseFromFile "function"
    addEither      <- parseFromFile "add"
    writeStrEither <- parseFromFile "writestr"
    hello2Either   <- parseFromFile "hello2"
    hello3Either   <- parseFromFile "hello3"
    hello4Either   <- parseFromFile "hello4"
    writeNumEither <- parseFromFile "writenum"
    multiplyEither <- parseFromFile "multiply"
    readNumEither  <- parseFromFile "readnum"
    factEither     <- parseFromFile "fact"
    bottlesEither  <- parseFromFile "bottles"
    euclidEither   <- parseFromFile "euclid"

    it "true"     $ do trueEither     `shouldBe` Right trueIL
    it "hello"    $ do helloEither    `shouldBe` Right helloIL
    it "pip"      $ do pipEither      `shouldBe` Right pipIL
    it "pip2"     $ do pip2Either     `shouldBe` Right pip2IL
    it "reverse"  $ do reverseEither  `shouldBe` Right reverseIL
    it "function" $ do functionEither `shouldBe` Right functionIL
    it "writestr" $ do writeStrEither `shouldBe` Right writeStrIL
    it "hello2"   $ do hello2Either   `shouldBe` Right (hello2IL ++ [D "writestr.eas"])
    it "hello3"   $ do hello3Either   `shouldBe` Right (hello2IL ++ [D "writestr.eas"])
    it "hello4"   $ do hello4Either   `shouldBe` Right (hello4IL ++ [D "writestr.eas"])
    it "writenum" $ do writeNumEither `shouldBe` Right writeNumIL
    it "multiply" $ do multiplyEither `shouldBe` Right multiplyIL
    it "readnum"  $ do readNumEither  `shouldBe` Right readNumIL
    it "fact"     $ do factEither     `shouldBe` Right (factIL   ++ [D "readnum.eas",D "writenum.eas",D "multiply.eas",D "writestr.eas"])
    it "bottles"  $ do bottlesEither  `shouldBe` Right bottlesIL
    it "euclid"   $ do euclidEither   `shouldBe` Right euclidIL

parseFromFile name = parseAssembler <$> runIO (readFileText $ "examples/eas/" <> name <> ".eas")
