module HelVM.HelPA.Assemblers.WSA.AsmParserSpec (spec) where

import           HelVM.HelPA.Assemblers.WSA.AsmParser
import           HelVM.HelPA.Assemblers.WSA.FileUtil
import           HelVM.HelPA.Assemblers.WSA.Instruction
import           HelVM.HelPA.Assemblers.WSA.TestData

import           HelVM.HelIO.Control.Safe

import           HelVM.Expectations
import           HelVM.GoldenExpectations

import           HelVM.HelPA.Assembler.Value

import           System.FilePath.Posix

import           Test.Hspec                             (Spec, describe, it)

spec :: Spec
spec = do
  let parseAssemblyFile fileName = parseAssemblyText <$> readFileText fileName
  let parseAssemblyLib  fileName = parseAssemblyFile (libDir </> fileName <.> lang)
  let parseAssemblyApp  fileName = parseAssemblyFile (appDir </> fileName <.> lang)
  let parseAssemblyEAS  fileName = parseAssemblyFile (wsaDir </> "from-eas" </> fileName <.> lang)

  describe "parseAssemblyLib" $
    forM_ [ "io"
          , "memory"
          ] $ \fileName ->
      it fileName $
        safeIOToPTextIO (parseAssemblyLib fileName) `goldenShouldIO` buildAbsolutePathToIlFile ("parseAssemblyLib" </> fileName)

  describe "parseAssemblyApp" $ do
    describe "original" $
      forM_ [ "prim"
            ] $ \fileName ->
        it fileName $
          safeIOToPTextIO (parseAssemblyApp fileName) `goldenShouldIO` buildAbsolutePathToIlFile ("parseAssemblyApp" </> "original" </> fileName)

    describe "from-eas" $
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
            ] $ \ fileName ->
        it fileName $
          safeIOToPTextIO (parseAssemblyEAS fileName) `goldenShouldIO` buildAbsolutePathToIlFile ("parseAssemblyApp" </> "from-eas" </> fileName)

  describe "parseAssemblyText" $ do
    it "io"     $ parseAssemblyLib "io"     `shouldSafeIO` ioIL
    it "memory" $ parseAssemblyLib "memory" `shouldSafeIO` memoryIL
    it "prim"   $ parseAssemblyApp "prim"   `shouldSafeIO` (Include "io" : primIL)

  describe "Commands without operands" $
    forM_ [ ("pop"  , [Pop])
          , ("doub" , [Dup])
          , ("swap" , [Swap])
          , ("ret"  , [Return])
          , ("exit" , [End])
          , ("outn" , [OutputNum])
          , ("outc" , [OutputChar])
          , ("inn"  , [InputNum])
          , ("inc"  , [InputChar])
          ] $ \(line , il) ->
      it line $ parseAssemblyText (toText line) `shouldSafe` il

  describe "Commands with maybe natural operand" $
    forM_ [ ( "add"       , [Add Nothing])
          , ( "add 0"     , [Add (Just (Literal 0))])
          , ( "add 1"     , [Add (Just (Literal 1))])

          , ( "sub"       , [Sub Nothing])
          , ( "sub 0"     , [Sub (Just (Literal 0))])
          , ( "sub 1"     , [Sub (Just (Literal 1))])

          , ( "mul"       , [Mul Nothing])
          , ( "mul 0"     , [Mul (Just (Literal 0))])
          , ( "mul 1"     , [Mul (Just (Literal 1))])

          , ( "div"       , [Div Nothing])
          , ( "div 0"     , [Div (Just (Literal 0))])
          , ( "div 1"     , [Div (Just (Literal 1))])

          , ( "mod"       , [Mod Nothing])
          , ( "mod 0"     , [Mod (Just (Literal 0))])
          , ( "mod 1"     , [Mod (Just (Literal 1))])

          , ( "store"     , [Store Nothing])
          , ( "store 0"   , [Store (Just (Literal 0))])
          , ( "store 1"   , [Store (Just (Literal 1))])

          , ( "retrive"   , [Load Nothing])
          , ( "retrive 0" , [Load (Just (Literal 0))])
          , ( "retrive 1" , [Load (Just (Literal 1))])
          ] $ \(line , il) ->
      it line $ parseAssemblyText (toText line) `shouldSafe` il

  describe "Commands with identifier operand" $
    forM_ [ ("label L"    , [Mark "L"])
          , ("call L"     , [Call "L"])
          , ("jump L"     , [Branch "L"])
          , ("jumpz L"    , [BranchZ "L"])
          , ("jumpn L"    , [BranchM "L"])
          , ("jumpp L"    , [BranchP "L"])
          , ("jumpnz L"   , [BranchNP "L"])
          , ("jumppz L"   , [BranchNM "L"])
          , ("include L"  , [Include "L"])
          ] $ \(line , il) ->
      it line $ parseAssemblyText (toText line) `shouldSafe` il

  describe "Commands with natural operand" $
    forM_ [ ("push 0" , [Push (Literal 0)])
          , ("push 1" , [Push (Literal 1)])
          , ("test 0" , [Test 0])
          , ("test 1" , [Test 1])
          ] $ \(line , il) ->
      it line $ parseAssemblyText (toText line) `shouldSafe` il

  describe "Commands with other operand" $
    forM_ [ ("pushs \"\""  , [PushS (Literal "")])
          , ("pushs \"0\"" , [PushS (Literal "0")])
          , ("pushs \"1\"" , [PushS (Literal "1")])
          ] $ \(line , il) ->
      it line $ parseAssemblyText (toText line) `shouldSafe` il

  describe "Comments" $
    forM_ [ (""                            , [])
          , (";"                           , [])
          , ("\n"                          , [])
          , (";\n"                         , [])
          , ("\n;"                         , [])
          , ("\n\n"                        , [])
          , ("label L;"                    , [Mark "L"])
          , ("label L;\n"                  , [Mark "L"])
          , ("label L\n;"                  , [Mark "L"])
          , ("label L\nlabel L\n"          , [Mark "L", Mark "L"])
          , ("label L\n;label L"           , [Mark "L"])
          , ("label L\n;label L\n"         , [Mark "L"])
          , ("label L\nlabel L\nlabel L\n" , [Mark "L", Mark "L", Mark "L"])
          ] $ \(line , il) ->
      it line $ parseAssemblyText (toText line) `shouldSafe` il
