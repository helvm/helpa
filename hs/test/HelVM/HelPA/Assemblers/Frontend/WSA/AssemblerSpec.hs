module HelVM.HelPA.Assemblers.Frontend.WSA.AssemblerSpec (spec) where

import           HelVM.HelPA.Assemblers.Backend.WSA.AssemblyOptionsExtra
import           HelVM.HelPA.Assemblers.Frontend.WSA.Assembler
import           HelVM.HelPA.Assemblers.Frontend.WSA.FileExtra

import           HelVM.HelIO.NamedValue
import           HelVM.HelPA.Assembler.API.SourcePath

import           HelVM.GoldenExpectations
import           HelVM.HelIO.CartesianProduct

import           System.FilePath.Posix

import           Test.Hspec                                              (Spec, describe, it)

spec :: Spec
spec = do
  let pathLib fileName = SourcePath {dirPath = libDir , filePath = libDir </> fileName <.> lang}
  let pathApp fileName = SourcePath {dirPath = libDir , filePath = appDir </> fileName <.> lang}

  describe "assembleLib" $
    forM_ ([ "io"
           , "memory"
           ] >*< manyOptionsWithName) $ \(fileName , namedOptions) -> do
      let options = value namedOptions
      let path = pathLib fileName
      let assembleLib = assembleFile options path
      let minorPath = name namedOptions </> fileName
      it minorPath $
        assembleLib `goldenShouldControlT` buildAbsolutePathToWsFile ("assembleLib" </> minorPath)

  describe "assembleApp" $ do
    describe "original" $
      forM_ ([ "prim"
             ] >*< manyOptionsWithName) $ \(fileName , namedOptions) -> do
        let options = value namedOptions
        let path = pathApp fileName
        let assembleApp = assembleFile options path
        let minorPath = name namedOptions </> fileName
        it minorPath $
          assembleApp `goldenShouldControlT` buildAbsolutePathToWsFile ("assembleApp" </> "original" </> minorPath)

    describe "from-eas" $
      forM_ ([ "true"
             , "hello"
             , "pip"
  --           , "pip2"
  --           , "reverse"
  --           , "function"
  --           , "add"
  --           , "writestr"
             , "hello2"
  --           , "hello3"
             , "hello4"
  ----           , "writenum"
  --           , "multiply"
  ----           , "readnum"
  --           , "fact"
             , "bottles"
  --           , "euclid"
              ] >*< manyOptionsWithName) $ \(fileName , namedOptions) -> do
        let path = SourcePath {dirPath = libDir , filePath = wsaDir </> "from-eas" </> fileName <.> lang}
        let options = value namedOptions
        let assemble = assembleFile options path
        let minorPath = name namedOptions </> fileName
        it minorPath $
          assemble `goldenShouldControlT` buildAbsolutePathToWsFile ("assembleApp" </> "from-eas" </> minorPath)
