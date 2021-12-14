module HelVM.HelPA.Assemblers.ASQ.AssemblerSpec where

import           HelVM.HelPA.Assembler.API.Separator
import           HelVM.HelPA.Assemblers.ASQ.API.QuestionMark

import           HelVM.HelPA.Assemblers.ASQ.Assembler
import           HelVM.HelPA.Assemblers.ASQ.AssemblyOptions
import           HelVM.HelPA.Assemblers.ASQ.FileUtil

import           HelVM.Expectations
import           HelVM.GoldenExpectations
import           HelVM.HelPA.Assembler.API.SourcePath

import           System.FilePath.Posix

import           Test.Hspec                                  (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "assembleFile" $ do
    it "True" $ do True `shouldBe` True
    forM_ [ (CurrentAddress , "esolangs" </> "helloWorld")
          , (NextAddress    , "mazonka"  </> "echo"      )
          , (NextAddress    , "mazonka"  </> "hi"        )
          , (NextAddress    , "mazonka"  </> "helloWorld")
          , (NextAddress    , "mazonka"  </> "siout"     )
          , (NextAddress    , "mazonka"  </> "si"        )
          , (NextAddress    , "mazonka"  </> "factorial" )
--          , (CurrentAddress , "current"  </> "99-bottles-of-beer")
--          , (CurrentAddress , "current"  </> "99-bottles-of-beer1")
--          , (NextAddress    , "next"     </> "99-bottles-of-beer")
--          , (NextAddress    , "next"     </> "99-bottles-of-beer1")
          ] $ \(address , fileName) -> do
      let options  = (AssemblyOptions {addOutLabel = False , separator = EOL , questionMark = address})
      let path     = SourcePath {dirPath = asqDir , filePath = buildAbsolutePathToAsqFile fileName}
      let assemble = assembleFile path options
      it fileName $ do assemble `goldenShouldSafeExceptT` buildAbsolutePathToSqFile ("assembleFile" </> fileName)

  describe "parseAssemblyText" $ do
    describe "CurrentAddress" $ do
      forM_ [ (";"         , "")
            , (". \"\\n\";", "10")
            , ("e: e;"     , "0 0 3")
            , ("e; e:;"    , "3 3 3")
            , ("? ? ?;"    , "0 1 2")
            , ("? ?;"      , "0 1 3")
            , ("?;"        , "0 0 3")
            , ("e ?;e:;"   , "3 1 3")
            ] $ \(line , sq) -> do
        let options = (AssemblyOptions {addOutLabel = False , separator = Space , questionMark = CurrentAddress})
        it line $ do assembleText (toText line) options `shouldSafe` sq
    describe "NextAddress" $ do
      forM_ [ (";"         , "")
            , (". \"\\n\";", "10")
            , ("e: e;"     , "0 0 3")
            , ("e; e:;"    , "3 3 3")
            , ("? ? ?;"    , "1 2 3")
            , ("? ?;"      , "1 2 3")
            , ("?;"        , "1 1 3")
            , ("e ?;e:;"   , "3 2 3")
            ] $ \(line , sq) -> do
        let options = (AssemblyOptions {addOutLabel = False , separator = Space , questionMark = NextAddress})
        it line $ do assembleText (toText line) options `shouldSafe` sq
