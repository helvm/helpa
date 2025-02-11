module HelVM.HelPA.Assemblers.Frontend.Piet.AsmParserSpec (spec) where

import           HelVM.HelPA.Assemblers.Frontend.Piet.AsmParser
import           HelVM.HelPA.Assemblers.Frontend.Piet.FileExtra
import           HelVM.HelPA.Assemblers.Frontend.Piet.Instruction

import           HelVM.HelPA.Assembler.Macro

import           HelVM.HelIO.Control.Safe

import           HelVM.HelIO.Extra

import           HelVM.Expectations
import           HelVM.GoldenExpectations

import           System.FilePath.Posix

import           Test.Hspec                                       (Spec, describe, it)

spec :: Spec
spec = do
  let parseAssemblyApp = "parseAssemblyApp"
  let parseAssemblyFile fileName = parseAssemblyText <$> readFileTextUtf8 fileName
  let parseAssemblyLang fileName = parseAssemblyFile (langDir </> fileName <.> lang)

  describe parseAssemblyApp $ do
      forM_ [ "fizzbuzz"
            , "readln"
            , "print"
            , "scan"
            , "_track_0"
            , "_track_1"
            , "_track_2"
            , "_track_3"
            , "_track_4"
            , "quest"
            ] $ \ fileName ->
        it fileName $
          safeIOToPTextIO (parseAssemblyLang fileName) `goldenShouldIO` buildAbsolutePathToIlFile (parseAssemblyApp </> fileName)

  describe "Directive " $
    forM_ [ ("1"                           , PushInteger 1)
          , ("2:"                          , Label (LNatural 2))
          , ("_track_0:"                   , Label (LIdentifier "_track_0"))
          , ("halt"                        , Halt)
          , (".track #1"                   , Track)
          , (".track #2"                   , Track)
          , (".track #3"                   , Track)
          , ("bz.0f"                       , Branch Nothing (Just BZ) (BLNatural (Just Forward) 0) )
          , (".btbl\n"                     , BranchTable [])
          , (".btbl _track_1\n"            , BranchTable [BLIdentifier "_track_1"])
          , (".btbl 0\n"                   , BranchTable [BLNatural Nothing 0])
          , (".btbl 0f\n"                  , BranchTable [BLNatural (Just Forward) 0])
          , ("b.entry"                     , Branch Nothing Nothing (BLIdentifier "entry") )
          , ("-1"                          , PushInteger (-1) )
          ] $ \(line , i) ->
      it line $ parseAssemblyText (toText line) `shouldSafe` [Micro $ Directive i]

  describe "Micro instruction list " $
    forM_ [ ("dup 15 mod bz._track_3"      , [Command Dup , Directive (PushInteger 15) , Command Mod , Directive (Branch Nothing (Just BZ) (BLIdentifier "_track_3"))])
          , ("dup 3 mod bz._track_1"       , [Command Dup , Directive (PushInteger 3) , Command Mod , Directive (Branch Nothing (Just BZ) (BLIdentifier "_track_1"))])
          , ("dup 5 mod bz._track_2"       , [Command Dup , Directive (PushInteger 5) , Command Mod , Directive (Branch Nothing (Just BZ) (BLIdentifier "_track_2") )])
          , ("dup outn 10 out"             , [Command Dup , Command OutN , Directive (PushInteger 10) , Command Out])
          , ("1 add dup 100 brle.2b"       , [Directive (PushInteger 1) , Command Add , Command Dup , Directive (PushInteger 100) , Directive (Branch Nothing (Just BLE) (BLNatural (Just Backward) 2) )])
          , ("@\"Fizz\n\" br._track_0"     , [Directive (Print "Fizz\n") , Directive (Branch Nothing Nothing (BLIdentifier "_track_0"))])
          , ("@\"Buzz\n\" br._track_0"     , [Directive (Print "Buzz\n") , Directive (Branch Nothing Nothing (BLIdentifier "_track_0"))])
          , ("@\"FizzBuzz\n\" br._track_0" , [Directive (Print "FizzBuzz\n") , Directive (Branch Nothing Nothing (BLIdentifier "_track_0") )])
          , ("dup 8 sub bz.1b.pop"         , [Command Dup, Directive (PushInteger 8), Command Sub, Directive (Branch (Just Pop) (Just BZ) (BLNatural (Just Backward) 1) )])
          , (".btbl _track_1 _track_2\n"   , [Directive (BranchTable [BLIdentifier "_track_1", BLIdentifier "_track_2"])])
          , (".btbl _track_1\n0"           , [Directive (BranchTable [BLIdentifier "_track_1"]), Directive (PushInteger 0)])
          , (".btbl _track_1\n0:"          , [Directive (BranchTable [BLIdentifier "_track_1"]), Directive (Label (LNatural 0))])
          , ("0: \"Please\" b.entry"       , [Directive (Label (LNatural 0)), Directive (PushString "Please"), Directive (Branch Nothing Nothing (BLIdentifier "entry") )])
          ] $ \(line , il) ->
      it line $ parseAssemblyText (toText line) `shouldSafe` (Micro <$> il)

  describe "Macro " $
    forM_ [ ("macro readln def"            , [Def [] "readln" []])
          , ("macro 0 br.0f readln def"    , [Def [] "readln" [Micro (Directive (PushInteger 0)),Micro (Directive (Branch Nothing Nothing (BLNatural (Just Forward) 0) ))]])
          ] $ \(line , il) ->
      it line $ parseAssemblyText (toText line) `shouldSafe` il
