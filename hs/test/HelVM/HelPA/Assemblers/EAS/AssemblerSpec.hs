module HelVM.HelPA.Assemblers.EAS.AssemblerSpec where

import HelVM.HelPA.Assemblers.EAS.Assembler
import HelVM.HelPA.Assemblers.EAS.FileUtil

import HelVM.Expectations

import HelVM.HelPA.Assembler.API

import System.FilePath.Posix

import Test.Hspec

spec :: Spec
spec = do
  describe "assembleFile" $ do
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
          ] $ \fileName -> do
      let assemble = assembleFile SourcePath {dirPath = easDir, filePath = buildAbsolutePathToEasFile fileName}
      it fileName $ do assemble `goldenShouldSafeReturn` buildAbsolutePathToEtaFile ("assembleFile" </> fileName)
