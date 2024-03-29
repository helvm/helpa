module HelVM.HelPA.Assemblers.Backend.ASQ.EsoLangs.AssemblerSpec where

import           HelVM.HelPA.Assemblers.Backend.ASQ.EsoLangs.Assembler

import           HelVM.HelPA.Assemblers.Backend.ASQ.API.AssemblyOptions
import           HelVM.HelPA.Assemblers.Backend.ASQ.API.QuestionMark

import           HelVM.HelPA.Assemblers.Backend.ASQ.AssemblyOptionsExtra
import           HelVM.HelPA.Assemblers.Backend.ASQ.FileExtra

import           HelVM.HelPA.Assembler.API.SourcePath

import           HelVM.Expectations
import           HelVM.GoldenExpectations

import           System.FilePath.Posix

import           Test.Hspec                                              (Spec, describe, it)

spec :: Spec
spec = do
  describe "assembleFile" $
    forM_ [ (CurrentAddress , "esolangs" </> "currentAddress" </> "helloWorld")
          , (NextAddress    , "esolangs" </> "nextAddress"    </> "echo"      )
          , (NextAddress    , "esolangs" </> "nextAddress"    </> "hi"        )
          , (NextAddress    , "esolangs" </> "nextAddress"    </> "helloWorld")
          , (NextAddress    , "esolangs" </> "nextAddress"    </> "siout"     )
          , (NextAddress    , "esolangs" </> "nextAddress"    </> "si"        )
          , (NextAddress    , "esolangs" </> "nextAddress"    </> "factorial" )
          ] $ \(address , fileName) -> do
      let options  = defaultAssemblyOptionsForTest {questionMark = address}
      let path     = SourcePath {dirPath = asqDir , filePath = buildAbsolutePathToAsqFile fileName}
      let assemble = assembleFile options path
      it fileName $ assemble `goldenShouldControlT` buildAbsolutePathToSqFile ("assembleFile" </> fileName)

  describe "assembleTextWithCurrentAddress" $ do
    describe "CurrentAddress" $ do
      let assembleTextWithCurrentAddress = assembleText defaultAssemblyOptions
      forM_ [ (";"         , "")
            , (". \"\\n\";", "10")
            , ("e: e;"     , "0 0 3")
            , ("e; e:;"    , "3 3 3")
            , ("? ? ?;"    , "0 1 2")
            , ("? ?;"      , "0 1 3")
            , ("?;"        , "0 0 3")
            , ("e ?;e:;"   , "3 1 3")
            ] $ \(line , sq) ->
        it line $ assembleTextWithCurrentAddress (toText line) `shouldSafe` sq

    describe "assembleTextWithNextAddress" $ do
      let assembleTextWithNextAddress = assembleText defaultAssemblyOptionsWithNextAddress
      forM_ [ (";"         , "")
            , (". \"\\n\";", "10")
            , ("e: e;"     , "0 0 3")
            , ("e; e:;"    , "3 3 3")
            , ("? ? ?;"    , "1 2 3")
            , ("? ?;"      , "1 2 3")
            , ("?;"        , "1 1 3")
            , ("e ?;e:;"   , "3 2 3")
            ] $ \(line , sq) ->
        it line $ assembleTextWithNextAddress (toText line) `shouldSafe` sq
