module HelVM.HelPA.Assemblers.Frontend.SblAsm.AsmParserSpec (spec) where

import           HelVM.HelPA.Assemblers.Frontend.SblAsm.AsmParser
import           HelVM.HelPA.Assemblers.Frontend.SblAsm.FileExtra
--import           HelVM.HelPA.Assemblers.Frontend.SblAsm.Instruction

import           HelVM.HelIO.Control.Safe

import           HelVM.HelIO.Extra

--import           HelVM.Expectations
import           HelVM.GoldenExpectations

import           System.FilePath.Posix

import           Test.Hspec                                       (Spec, describe, it)

spec :: Spec
spec = do
  let parseAssemblyApp = "parseAssemblyApp" </> "sblasm"
  let examples = "examples"
--  let fixtures = "fixtures"
  let parseAssemblyFile fileName = parseAssemblyText <$> readFileTextUtf8 fileName
  let parseAssemblyExample fileName = parseAssemblyFile (asqDir </> examples </> fileName <.> lang)
--  let parseAssemblyFixture fileName = parseAssemblyFile (asqDir </> fixtures </> fileName <.> lang)

  describe parseAssemblyApp $ do
    describe examples $
      forM_ [ "echo"
            , "fizzbuzz"
            , "io.test"
            , "msg_macros"
            , "rock_paper_scissors"
            , "sble.test"
            , "standard.test"
            , "test.test"
            ] $ \fileName ->
        it fileName $
          safeIOToPTextIO (parseAssemblyExample fileName) `goldenShouldIO` buildAbsolutePathToIlFile (parseAssemblyApp </> examples </> fileName)

--    describe fixtures" $
--      forM_ [ "true"
--            , "hello"
--            , "pip"
--            ] $ \ fileName ->
--        it fileName $
--          safeIOToPTextIO (parseAssemblyFixture fileName) `goldenShouldIO` buildAbsolutePathToIlFile (parseAssemblyApp </> fixtures </> fileName)

