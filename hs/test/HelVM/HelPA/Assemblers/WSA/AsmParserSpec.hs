module HelVM.HelPA.Assemblers.WSA.AsmParserSpec (spec) where

import HelVM.HelPA.Assemblers.WSA.AsmParser
import HelVM.HelPA.Assemblers.WSA.Instruction
import HelVM.HelPA.Assemblers.WSA.TestData
import HelVM.HelPA.Assemblers.WSA.FileUtil

import HelVM.Expectations

import HelVM.HelPA.Assembler.Value

import System.FilePath.Posix

import Test.Hspec

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
        parseAssemblyLib fileName `goldenShouldSafeReturn` buildAbsolutePathToIlFile ("parseAssemblyLib" </> fileName)

  describe "parseAssemblyApp" $ do
    describe "original" $ do
      forM_ [ "prim"
            ] $ \fileName -> do
        it fileName $ do
          parseAssemblyApp fileName `goldenShouldSafeReturn` buildAbsolutePathToIlFile ("parseAssemblyApp" </> "original" </> fileName)

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
          parseAssemblyApp1 fileName `goldenShouldSafeReturn` buildAbsolutePathToIlFile ("parseAssemblyApp" </> "from-eas" </> fileName)

  describe "parseAssemblyText" $ do
    it "io"     $ do parseAssemblyLib "io"     `shouldSafeReturn` ioIL
    it "memory" $ do parseAssemblyLib "memory" `shouldSafeReturn` memoryIL
    it "prim"   $ do parseAssemblyApp "prim"   `shouldSafeReturn` (Include "io" : primIL)

  describe "Commands without operands" $ do
    it "parse 'pop'"  $ do parseAssemblyText "pop"  `shouldSafe` [Pop]
    it "parse 'doub'" $ do parseAssemblyText "doub" `shouldSafe` [Dup]
    it "parse 'swap'" $ do parseAssemblyText "swap" `shouldSafe` [Swap]
    it "parse 'ret'"  $ do parseAssemblyText "ret"  `shouldSafe` [Return]
    it "parse 'exit'" $ do parseAssemblyText "exit" `shouldSafe` [End]
    it "parse 'outn'" $ do parseAssemblyText "outn" `shouldSafe` [OutputNum]
    it "parse 'outc'" $ do parseAssemblyText "outc" `shouldSafe` [OutputChar]
    it "parse 'inn'"  $ do parseAssemblyText "inn"  `shouldSafe` [InputNum]
    it "parse 'inc'"  $ do parseAssemblyText "inc"  `shouldSafe` [InputChar]

  describe "Commands with maybe natural operand" $ do
    it "parse 'add'"   $ do parseAssemblyText "add"   `shouldSafe` [Add Nothing]
    it "parse 'add 0'" $ do parseAssemblyText "add 0" `shouldSafe` [Add (Just (Literal 0))]
    it "parse 'add 1'" $ do parseAssemblyText "add 1" `shouldSafe` [Add (Just (Literal 1))]

    it "parse 'sub'"   $ do parseAssemblyText "sub"   `shouldSafe` [Sub Nothing]
    it "parse 'sub 0'" $ do parseAssemblyText "sub 0" `shouldSafe` [Sub (Just (Literal 0))]
    it "parse 'sub 1'" $ do parseAssemblyText "sub 1" `shouldSafe` [Sub (Just (Literal 1))]

    it "parse 'mul'"   $ do parseAssemblyText "mul"   `shouldSafe` [Mul Nothing]
    it "parse 'mul 0'" $ do parseAssemblyText "mul 0" `shouldSafe` [Mul (Just (Literal 0))]
    it "parse 'mul 1'" $ do parseAssemblyText "mul 1" `shouldSafe` [Mul (Just (Literal 1))]

    it "parse 'div'"   $ do parseAssemblyText "div"   `shouldSafe` [Div Nothing]
    it "parse 'div 0'" $ do parseAssemblyText "div 0" `shouldSafe` [Div (Just (Literal 0))]
    it "parse 'div 1'" $ do parseAssemblyText "div 1" `shouldSafe` [Div (Just (Literal 1))]

    it "parse 'mod'"   $ do parseAssemblyText "mod"   `shouldSafe` [Mod Nothing]
    it "parse 'mod 0'" $ do parseAssemblyText "mod 0" `shouldSafe` [Mod (Just (Literal 0))]
    it "parse 'mod 1'" $ do parseAssemblyText "mod 1" `shouldSafe` [Mod (Just (Literal 1))]

    it "parse 'store'"     $ do parseAssemblyText "store"     `shouldSafe` [Store Nothing]
    it "parse 'store 0'"   $ do parseAssemblyText "store 0"   `shouldSafe` [Store (Just (Literal 0))]
    it "parse 'store 1'"   $ do parseAssemblyText "store 1"   `shouldSafe` [Store (Just (Literal 1))]

    it "parse 'retrive'"   $ do parseAssemblyText "retrive"   `shouldSafe` [Load Nothing]
    it "parse 'retrive 0'" $ do parseAssemblyText "retrive 0" `shouldSafe` [Load (Just (Literal 0))]
    it "parse 'retrive 1'" $ do parseAssemblyText "retrive 1" `shouldSafe` [Load (Just (Literal 1))]

  describe "Commands with identifier operand" $ do
    it "parse 'label L'"   $ do parseAssemblyText "label L"   `shouldSafe` [Mark "L"]
    it "parse 'call L'"    $ do parseAssemblyText "call L"    `shouldSafe` [Call "L"]
    it "parse 'jump L'"    $ do parseAssemblyText "jump L"    `shouldSafe` [Branch "L"]
    it "parse 'jumpz L'"   $ do parseAssemblyText "jumpz L"   `shouldSafe` [BranchZ "L"]
    it "parse 'jumpn L'"   $ do parseAssemblyText "jumpn L"   `shouldSafe` [BranchM "L"]
    it "parse 'jumpp L'"   $ do parseAssemblyText "jumpp L"   `shouldSafe` [BranchP "L"]
    it "parse 'jumpnz L'"  $ do parseAssemblyText "jumpnz L"  `shouldSafe` [BranchNP "L"]
    it "parse 'jumppz L'"  $ do parseAssemblyText "jumppz L"  `shouldSafe` [BranchNM "L"]
    it "parse 'include L'" $ do parseAssemblyText "include L" `shouldSafe` [Include "L"]

  describe "Commands with natural operand" $ do
    it "parse 'push 0'" $ do parseAssemblyText "push 0" `shouldSafe` [Push (Literal 0)]
    it "parse 'push 1'" $ do parseAssemblyText "push 1" `shouldSafe` [Push (Literal 1)]
    it "parse 'test 0'" $ do parseAssemblyText "test 0" `shouldSafe` [Test 0]
    it "parse 'test 1'" $ do parseAssemblyText "test 1" `shouldSafe` [Test 1]

  describe "Commands with other operand" $ do
    it "parse 'pushs \"\"'"  $ do parseAssemblyText "pushs \"\""  `shouldSafe` [PushS (Literal "")]
    it "parse 'pushs \"0\"'" $ do parseAssemblyText "pushs \"0\"" `shouldSafe` [PushS (Literal "0")]
    it "parse 'pushs \"1\"'" $ do parseAssemblyText "pushs \"1\"" `shouldSafe` [PushS (Literal "1")]

  describe "Comments" $ do
    it "parse ''"                           $ do parseAssemblyText ""                            `shouldSafe` []
    it "parse ';'"                          $ do parseAssemblyText ";"                           `shouldSafe` []
    it "parse '\n'"                         $ do parseAssemblyText "\n"                          `shouldSafe` []
    it "parse ';\n'"                        $ do parseAssemblyText ";\n"                         `shouldSafe` []
    it "parse '\n;'"                        $ do parseAssemblyText "\n;"                         `shouldSafe` []
    it "parse '\n\n'"                       $ do parseAssemblyText "\n\n"                        `shouldSafe` []
    it "parse 'label L;"                    $ do parseAssemblyText "label L;"                    `shouldSafe` [Mark "L"]
    it "parse 'label L;\n"                  $ do parseAssemblyText "label L;\n"                  `shouldSafe` [Mark "L"]
    it "parse 'label L\n;"                  $ do parseAssemblyText "label L\n;"                  `shouldSafe` [Mark "L"]
    it "parse 'label L\nlabel L\n"          $ do parseAssemblyText "label L\nlabel L\n"          `shouldSafe` [Mark "L", Mark "L"]
    it "parse 'label L\n;label L"           $ do parseAssemblyText "label L\n;label L"           `shouldSafe` [Mark "L"]
    it "parse 'label L\n;label L\n"         $ do parseAssemblyText "label L\n;label L\n"         `shouldSafe` [Mark "L"]
    it "parse 'label L\nlabel L\nlabel L\n" $ do parseAssemblyText "label L\nlabel L\nlabel L\n" `shouldSafe` [Mark "L", Mark "L", Mark "L"]
