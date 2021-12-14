module HelVM.HelPA.Assemblers.ASQ.Eigenratios.AsmParserSpec (spec) where

import           HelVM.HelPA.Assemblers.ASQ.Eigenratios.AsmParser

import           HelVM.HelPA.Assemblers.ASQ.FileUtil

import           HelVM.Common.Safe

import           HelVM.GoldenExpectations

import           System.FilePath.Posix

import           Test.Hspec                                       (Spec, describe, it)

spec :: Spec
spec = do

  describe "parseAssemblyFile" $ do
    let parseAssemblyFile fileName = parseAssemblyText <$> readFileText fileName
    forM_ [ "eigenratios" </> "si1"
          , "eigenratios" </> "si2"
          ] $ \fileName -> do
      let parseAssembly = parseAssemblyFile $ buildAbsolutePathToAsqFile fileName
      it fileName $ do safeIOToPTextIO parseAssembly `goldenShouldIO` buildAbsolutePathToIlFile ("parseAssemblyFile" </> fileName)
