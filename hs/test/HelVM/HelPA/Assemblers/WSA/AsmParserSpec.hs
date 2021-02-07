module HelVM.HelPA.Assemblers.WSA.AsmParserSpec (spec) where

import HelVM.HelPA.Assemblers.WSA.AsmParser
import HelVM.HelPA.Assemblers.WSA.Instruction
import HelVM.HelPA.Assemblers.WSA.TestData
import HelVM.HelPA.Assemblers.WSA.FileUtil

import HelVM.HelPA.Assemblers.Expectations

import HelVM.HelPA.Common.Value

import System.FilePath.Posix

import Test.Hspec
import Test.Hspec.Attoparsec

spec :: Spec
spec = do
  let parseAssemblyFile = fmap parseAssemblyText . readFileText

  let parseAssemblyLib = \ fileName -> parseAssemblyFile (libDir </> fileName <.> ext)
  let parseAssemblyApp = \ fileName -> parseAssemblyFile (appDir </> fileName <.> ext)
  let parseAssemblyApp1 = \ fileName -> parseAssemblyFile (wsaDir </> "from-eas" </> fileName <.> ext)

  describe "parseAssemblyLib" $ do
    forM_ [ "io"
          , "memory"
          ] $ \fileName -> do
      it fileName $ do
        parseAssemblyLib fileName `goldenShouldParseReturn` buildAbsolutePathToIlFile ("parseAssemblyLib" </> fileName)

  describe "parseAssemblyApp" $ do
    describe "original" $ do
      forM_ [ "prim"
            ] $ \fileName -> do
        it fileName $ do
          parseAssemblyApp fileName `goldenShouldParseReturn` buildAbsolutePathToIlFile ("parseAssemblyApp" </> "original" </> fileName)

    describe "from-eas" $ do
      forM_ [ "true"
            , "hello"
            , "pip"
  --           , "pip2"
  --           , "reverse"
  --           , "function"
  --           , "add"
  --           , "writestr"
            , "hello2"
  --          , "hello3"
            , "hello4"
  ----          , "writenum"
  --          , "multiply"
  ----          , "readnum"
  --          , "fact"
            , "bottles"
 --           , "euclid"
            ] $ \ fileName -> do
        it fileName $ do
          parseAssemblyApp1 fileName `goldenShouldParseReturn` buildAbsolutePathToIlFile ("parseAssemblyApp" </> "from-eas" </> fileName)

  describe "parseAssemblyText" $ do
    it "io"     $ do parseAssemblyLib "io"     `shouldParseReturn` ioIL
    it "memory" $ do parseAssemblyLib "memory" `shouldParseReturn` memoryIL
    it "prim"   $ do parseAssemblyApp "prim"   `shouldParseReturn` (Include "io" : primIL)

  describe "Commands without operands" $ do
    it "parse 'pop'"  $ do parseAssemblyText "pop"  `shouldParse` [Pop]
    it "parse 'doub'" $ do parseAssemblyText "doub" `shouldParse` [Dup]
    it "parse 'swap'" $ do parseAssemblyText "swap" `shouldParse` [Swap]
    it "parse 'ret'"  $ do parseAssemblyText "ret"  `shouldParse` [Return]
    it "parse 'exit'" $ do parseAssemblyText "exit" `shouldParse` [End]
    it "parse 'outn'" $ do parseAssemblyText "outn" `shouldParse` [OutputNum]
    it "parse 'outc'" $ do parseAssemblyText "outc" `shouldParse` [OutputChar]
    it "parse 'inn'"  $ do parseAssemblyText "inn"  `shouldParse` [InputNum]
    it "parse 'inc'"  $ do parseAssemblyText "inc"  `shouldParse` [InputChar]

  describe "Commands with maybe natural operand" $ do
    it "parse 'add'"   $ do parseAssemblyText "add"   `shouldParse` [Add Nothing]
    it "parse 'add 0'" $ do parseAssemblyText "add 0" `shouldParse` [Add (Just (Literal 0))]
    it "parse 'add 1'" $ do parseAssemblyText "add 1" `shouldParse` [Add (Just (Literal 1))]

    it "parse 'sub'"   $ do parseAssemblyText "sub"   `shouldParse` [Sub Nothing]
    it "parse 'sub 0'" $ do parseAssemblyText "sub 0" `shouldParse` [Sub (Just (Literal 0))]
    it "parse 'sub 1'" $ do parseAssemblyText "sub 1" `shouldParse` [Sub (Just (Literal 1))]

    it "parse 'mul'"   $ do parseAssemblyText "mul"   `shouldParse` [Mul Nothing]
    it "parse 'mul 0'" $ do parseAssemblyText "mul 0" `shouldParse` [Mul (Just (Literal 0))]
    it "parse 'mul 1'" $ do parseAssemblyText "mul 1" `shouldParse` [Mul (Just (Literal 1))]

    it "parse 'div'"   $ do parseAssemblyText "div"   `shouldParse` [Div Nothing]
    it "parse 'div 0'" $ do parseAssemblyText "div 0" `shouldParse` [Div (Just (Literal 0))]
    it "parse 'div 1'" $ do parseAssemblyText "div 1" `shouldParse` [Div (Just (Literal 1))]

    it "parse 'mod'"   $ do parseAssemblyText "mod"   `shouldParse` [Mod Nothing]
    it "parse 'mod 0'" $ do parseAssemblyText "mod 0" `shouldParse` [Mod (Just (Literal 0))]
    it "parse 'mod 1'" $ do parseAssemblyText "mod 1" `shouldParse` [Mod (Just (Literal 1))]

    it "parse 'store'"     $ do parseAssemblyText "store"     `shouldParse` [Store Nothing]
    it "parse 'store 0'"   $ do parseAssemblyText "store 0"   `shouldParse` [Store (Just (Literal 0))]
    it "parse 'store 1'"   $ do parseAssemblyText "store 1"   `shouldParse` [Store (Just (Literal 1))]

    it "parse 'retrive'"   $ do parseAssemblyText "retrive"   `shouldParse` [Load Nothing]
    it "parse 'retrive 0'" $ do parseAssemblyText "retrive 0" `shouldParse` [Load (Just (Literal 0))]
    it "parse 'retrive 1'" $ do parseAssemblyText "retrive 1" `shouldParse` [Load (Just (Literal 1))]

  describe "Commands with identifier operand" $ do
    it "parse 'label L'"   $ do parseAssemblyText "label L"   `shouldParse` [Mark "L"]
    it "parse 'call L'"    $ do parseAssemblyText "call L"    `shouldParse` [Call "L"]
    it "parse 'jump L'"    $ do parseAssemblyText "jump L"    `shouldParse` [Branch "L"]
    it "parse 'jumpz L'"   $ do parseAssemblyText "jumpz L"   `shouldParse` [BranchZ "L"]
    it "parse 'jumpn L'"   $ do parseAssemblyText "jumpn L"   `shouldParse` [BranchM "L"]
    it "parse 'jumpp L'"   $ do parseAssemblyText "jumpp L"   `shouldParse` [BranchP "L"]
    it "parse 'jumpnz L'"  $ do parseAssemblyText "jumpnz L"  `shouldParse` [BranchNP "L"]
    it "parse 'jumppz L'"  $ do parseAssemblyText "jumppz L"  `shouldParse` [BranchNM "L"]
    it "parse 'include L'" $ do parseAssemblyText "include L" `shouldParse` [Include "L"]

  describe "Commands with natural operand" $ do
    it "parse 'push 0'" $ do parseAssemblyText "push 0" `shouldParse` [Push (Literal 0)]
    it "parse 'push 1'" $ do parseAssemblyText "push 1" `shouldParse` [Push (Literal 1)]
    it "parse 'test 0'" $ do parseAssemblyText "test 0" `shouldParse` [Test 0]
    it "parse 'test 1'" $ do parseAssemblyText "test 1" `shouldParse` [Test 1]

  describe "Commands with other operand" $ do
    it "parse 'pushs \"\"'"  $ do parseAssemblyText "pushs \"\""  `shouldParse` [PushS (Literal "")]
    it "parse 'pushs \"0\"'" $ do parseAssemblyText "pushs \"0\"" `shouldParse` [PushS (Literal "0")]
    it "parse 'pushs \"1\"'" $ do parseAssemblyText "pushs \"1\"" `shouldParse` [PushS (Literal "1")]

  describe "Comments" $ do
    it "parse ''"                           $ do parseAssemblyText ""                            `shouldParse` []
    it "parse ';'"                          $ do parseAssemblyText ";"                           `shouldParse` []
    it "parse '\n'"                         $ do parseAssemblyText ";"                           `shouldParse` []
    it "parse ';\n'"                        $ do parseAssemblyText ";"                           `shouldParse` []
    it "parse '\n;'"                        $ do parseAssemblyText ";"                           `shouldParse` []
    it "parse 'label L;"                    $ do parseAssemblyText "label L;"                    `shouldParse` [Mark "L"]
    it "parse 'label L;\n"                  $ do parseAssemblyText "label L;\n"                  `shouldParse` [Mark "L"]
    it "parse 'label L\n;"                  $ do parseAssemblyText "label L\n;"                  `shouldParse` [Mark "L"]
    it "parse 'label L\nlabel L\n"          $ do parseAssemblyText "label L\nlabel L\n"          `shouldParse` [Mark "L", Mark "L"]
    it "parse 'label L\n;label L"           $ do parseAssemblyText "label L\n;label L"           `shouldParse` [Mark "L"]
    it "parse 'label L\n;label L\n"         $ do parseAssemblyText "label L\n;label L\n"         `shouldParse` [Mark "L"]
    it "parse 'label L\nlabel L\nlabel L\n" $ do parseAssemblyText "label L\nlabel L\nlabel L\n" `shouldParse` [Mark "L", Mark "L", Mark "L"]
