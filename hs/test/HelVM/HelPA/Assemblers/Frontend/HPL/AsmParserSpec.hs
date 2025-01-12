module HelVM.HelPA.Assemblers.Frontend.HPL.AsmParserSpec (spec) where

import           HelVM.HelPA.Assemblers.Frontend.HPL.AsmParser
import           HelVM.HelPA.Assemblers.Frontend.HPL.FileExtra
import           HelVM.HelPA.Assemblers.Frontend.HPL.Instruction
--import           HelVM.HelPA.Assemblers.Frontend.HPL.TestData

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
  let parseAssemblyApp2  fileName = parseAssemblyFile (app2Dir </> fileName <.> lang)

  describe "parseAssemblyLib" $
    forM_ [ "base"
          ] $ \fileName ->
      it fileName $
        safeIOToPTextIO (parseAssemblyLib fileName) `goldenShouldIO` buildAbsolutePathToIlFile ("parseAssemblyLib" </> fileName)

  describe "parseAssemblyApp" $ do
    describe "tutorial" $
      forM_ [ "hello"
            ] $ \fileName ->
        it fileName $
          safeIOToPTextIO (parseAssemblyApp fileName) `goldenShouldIO` buildAbsolutePathToIlFile ("parseAssemblyApp" </> "original" </> fileName)

--    describe "programs" $
--      forM_ [ "true"
--            , "hello"
--            , "pip"
--  --           , "pip2"
--  --           , "reverse"
--  --           , "function"
--  --           , "add"
--  --           , "writestr"
--            , "hello2"
--  --          , "hello3"
--            , "hello4"
--  ----          , "writenum"
--  --          , "multiply"
--  ----          , "readnum"
--  --          , "fact"
--            , "bottles"
-- --           , "euclid"
--            ] $ \ fileName ->
--        it fileName $
--          safeIOToPTextIO (parseAssemblyEAS fileName) `goldenShouldIO` buildAbsolutePathToIlFile ("parseAssemblyApp" </> "from-eas" </> fileName)
--
