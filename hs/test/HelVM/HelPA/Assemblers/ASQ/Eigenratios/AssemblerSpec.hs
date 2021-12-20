module HelVM.HelPA.Assemblers.ASQ.Eigenratios.AssemblerSpec where

import           HelVM.HelPA.Assemblers.ASQ.Eigenratios.Assembler

import           HelVM.HelPA.Assemblers.ASQ.AssemblyOptionsUtil
import           HelVM.HelPA.Assemblers.ASQ.FileUtil

import           HelVM.HelPA.Assembler.API.SourcePath

import           HelVM.GoldenExpectations

import           System.FilePath.Posix

import           Test.Hspec                                       (Spec, describe, it)

spec :: Spec
spec = do
  describe "assembleFile" $ do
    let assembleForTest = assembleFile defaultAssemblyOptionsForTest
    forM_ [ "eigenratios" </> "si1"
          , "eigenratios" </> "si2"
          ] $ \fileName -> do
      let path     = SourcePath {dirPath = asqDir , filePath = buildAbsolutePathToAsqFile fileName}
      it fileName $ do assembleForTest path `goldenShouldSafeExceptT` buildAbsolutePathToSqFile ("assembleFile" </> fileName)
