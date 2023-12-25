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
  let parseAssemblyApp = "parseAssemblyApp"
  let fromElvmName = "from-elvm"
  let fromOnlineName = "from-online"
  let parseAssemblyFile fileName = parseAssemblyText <$> readFileTextUtf8 fileName
  let parseAssemblyELVM fileName = parseAssemblyFile (wsaDir </> fromElvmName </> fileName <.> lang)
  let parseAssemblyOnline fileName = parseAssemblyFile (wsaDir </> fromOnlineName </> fileName <.> lang)

  describe parseAssemblyApp $ do
    describe fromElvmName $
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
          safeIOToPTextIO (parseAssemblyELVM fileName) `goldenShouldIO` buildAbsolutePathToIlFile (parseAssemblyApp </> fromElvmName </> fileName)
    describe "from-online" $
      forM_ [ "helloworld"
            , "fizzbuzz"
            ] $ \ fileName ->
        it fileName $
          safeIOToPTextIO (parseAssemblyOnline fileName) `goldenShouldIO` buildAbsolutePathToIlFile (parseAssemblyApp </> fromOnlineName </> fileName)

  describe "Commands" $
    forM_ [ ("mov A, 43" , [Mov "A" (Literal 43)])
          , ("jmp A", [Jmp "A"])
          ] $ \(line , il) ->
      it line $ parseAssemblyText (toText line) `shouldSafe` il
