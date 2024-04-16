module HelVM.HelPA.Assemblers.Frontend.WSA.AsmParserSpec (spec) where

import           HelVM.HelPA.Assemblers.Frontend.WSA.AsmParser
import           HelVM.HelPA.Assemblers.Frontend.WSA.FileExtra
import           HelVM.HelPA.Assemblers.Frontend.WSA.Instruction
import           HelVM.HelPA.Assemblers.Frontend.WSA.TestData

import           HelVM.HelIO.Control.Safe

import           HelVM.HelIO.Extra

import           HelVM.Expectations
import           HelVM.GoldenExpectations

import           System.FilePath.Posix

import           Test.Hspec                                      (Spec, describe, it)

spec :: Spec
spec = do
  let parseAssemblyFile fileName = parseAssemblyText <$> readFileTextUtf8 fileName
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
          , ( "add 0"     , [Add (Just 0)])
          , ( "add 1"     , [Add (Just 1)])

          , ( "sub"       , [Sub Nothing])
          , ( "sub 0"     , [Sub (Just 0)])
          , ( "sub 1"     , [Sub (Just 1)])

          , ( "mul"       , [Mul Nothing])
          , ( "mul 0"     , [Mul (Just 0)])
          , ( "mul 1"     , [Mul (Just 1)])

          , ( "div"       , [Div Nothing])
          , ( "div 0"     , [Div (Just 0)])
          , ( "div 1"     , [Div (Just 1)])

          , ( "mod"       , [Mod Nothing])
          , ( "mod 0"     , [Mod (Just 0)])
          , ( "mod 1"     , [Mod (Just 1)])

          , ( "store"     , [Store Nothing])
          , ( "store 0"   , [Store (Just 0)])
          , ( "store 1"   , [Store (Just 1)])

          , ( "retrive"   , [Load Nothing])
          , ( "retrive 0" , [Load (Just 0)])
          , ( "retrive 1" , [Load (Just 1)])
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
    forM_ [ ("push 0" , [Push 0])
          , ("push 1" , [Push 1])
          , ("test 0" , [Test 0])
          , ("test 1" , [Test 1])
          ] $ \(line , il) ->
      it line $ parseAssemblyText (toText line) `shouldSafe` il

  describe "Commands with other operand" $
    forM_ [ ("pushs \"\""  , [PushS ""])
          , ("pushs \"0\"" , [PushS "0"])
          , ("pushs \"1\"" , [PushS "1"])
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
