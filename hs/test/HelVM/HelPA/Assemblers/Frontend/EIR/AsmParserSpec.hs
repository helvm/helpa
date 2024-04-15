module HelVM.HelPA.Assemblers.Frontend.EIR.AsmParserSpec (spec) where

import           HelVM.HelPA.Assemblers.Frontend.EIR.AsmParser
import           HelVM.HelPA.Assemblers.Frontend.EIR.FileExtra
import           HelVM.HelPA.Assemblers.Frontend.EIR.Instruction

import           HelVM.HelPA.Assembler.Value

import           HelVM.HelIO.Control.Safe
import           HelVM.HelIO.Extra

import           HelVM.Expectations
import           HelVM.GoldenExpectations

import           System.FilePath.Posix

import           Test.Hspec                                      (Spec, describe, it)

spec :: Spec
spec = do
  let parseAssemblyFile fileName = parseAssemblyText <$> readFileTextUtf8 fileName
  let parseAssemblyELVM fileName = parseAssemblyFile (wsaDir </> "from-elvm" </> fileName <.> lang)
  let parseAssemblyOnline fileName = parseAssemblyFile (wsaDir </> "from-online" </> fileName <.> lang)

  describe "parseAssemblyApp" $ do
    describe "from-elvm" $
      forM_ [ "00exit"
            , "01putc"
            , "02mov"
            , "03mov_reg"
            , "04getc"
            , "05regjmp"
            , "06mem"
            , "07mem"
            , "08data"
            , "add_self"
            , "basic"
            , "bug_cmp"
            , "echo"
            , "sub"
            , "sub_bug"
            ] $ \ fileName ->
        it fileName $
          safeIOToPTextIO (parseAssemblyELVM fileName) `goldenShouldIO` buildAbsolutePathToIlFile ("parseAssemblyApp" </> "from-elvm" </> fileName)
    describe "from-online" $
      forM_ [ "helloworld"
            , "fizzbuzz"
            ] $ \ fileName ->
        it fileName $
          safeIOToPTextIO (parseAssemblyOnline fileName) `goldenShouldIO` buildAbsolutePathToIlFile ("parseAssemblyApp" </> "from-online" </> fileName)

  describe "Commands" $
    forM_ [ ("mov A, 43" , [Mov "A" (Literal 43)])
          , ("jmp A", [Jmp "A"])
          ] $ \(line , il) ->
      it line $ parseAssemblyText (toText line) `shouldSafe` il
