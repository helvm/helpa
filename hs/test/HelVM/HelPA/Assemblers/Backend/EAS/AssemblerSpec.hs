module HelVM.HelPA.Assemblers.Backend.EAS.AssemblerSpec where

import           HelVM.HelPA.Assemblers.Backend.EAS.Assembler
import           HelVM.HelPA.Assemblers.Backend.EAS.FileExtra

import           HelVM.GoldenExpectations

import           HelVM.HelPA.Assembler.API.SourcePath
import           System.FilePath.Posix

import           Test.Hspec                                   (Spec, describe, it)

spec :: Spec
spec =
  describe "assembleFile" $
    forM_ [ "true"
          , "hello"
          , "pip"
          , "pip2"
          , "reverse"
          , "function"
          , "add"
          , "writestr"
          , "hello2"
          , "hello3"
          , "hello4"
--          , "writenum"
          , "multiply"
--          , "readnum"
          , "fact"
          , "factN1H"
          , "bottles"
          , "bottles2"
          , "bottles3"
          , "euclid"
          , "divmod"
          ] $ \fileName -> do
      let path = SourcePath {dirPath = easDir , filePath = buildAbsolutePathToEasFile fileName}
      let assemble = assembleFile path
      it fileName $ assemble `goldenShouldControlT` buildAbsolutePathToEtaFile ("assembleFile" </> fileName)
