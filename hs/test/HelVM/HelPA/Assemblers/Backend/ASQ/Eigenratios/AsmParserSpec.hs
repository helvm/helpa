module HelVM.HelPA.Assemblers.Backend.ASQ.Eigenratios.AsmParserSpec (spec) where

import           HelVM.HelPA.Assemblers.Backend.ASQ.Eigenratios.AsmParser

import           HelVM.HelPA.Assemblers.Backend.ASQ.FileExtra

import           HelVM.HelIO.Control.Safe
import           HelVM.HelIO.Extra

import           HelVM.GoldenExpectations

import           System.FilePath.Posix

import           Test.Hspec                                               (Spec, describe, it)

spec :: Spec
spec =
  describe "parseAssemblyApp" $ do
    let parseAssemblyFile fileName = parseAssemblyText <$> readFileTextUtf8 fileName
    forM_ [ "eigenratios" </> "si1"
          , "eigenratios" </> "si2"
          ] $ \fileName -> do
      let parseAssembly = parseAssemblyFile $ buildAbsolutePathToAsqFile fileName
      it fileName $ safeIOToPTextIO parseAssembly `goldenShouldIO` buildAbsolutePathToIlFile ("parseAssemblyApp" </> fileName)
