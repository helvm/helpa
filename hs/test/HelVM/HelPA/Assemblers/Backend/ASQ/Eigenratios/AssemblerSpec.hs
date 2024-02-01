module HelVM.HelPA.Assemblers.Backend.ASQ.Eigenratios.AssemblerSpec where

import           HelVM.HelPA.Assemblers.Backend.ASQ.Eigenratios.Assembler

import           HelVM.HelPA.Assemblers.Backend.ASQ.AssemblyOptionsExtra
import           HelVM.HelPA.Assemblers.Backend.ASQ.FileExtra

import           HelVM.HelPA.Assembler.API.SourcePath

import           HelVM.GoldenExpectations

import           System.FilePath.Posix

import           Test.Hspec                                               (Spec, describe, it)

spec :: Spec
spec =
  describe "assembleFile" $ do
    let assembleForTest = assembleFile defaultAssemblyOptionsForTest
    forM_ [ "eigenratios" </> "si1"
          , "eigenratios" </> "si2"
          ] $ \fileName -> do
      let path = SourcePath {dirPath = asqDir , filePath = buildAbsolutePathToAsqFile fileName}
      it fileName $ assembleForTest path `goldenShouldControlT` buildAbsolutePathToSqFile ("assembleFile" </> fileName)
