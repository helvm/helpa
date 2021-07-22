module HelVM.HelPA.Assemblers.EAS.ReducerSpec where

import           HelVM.HelPA.Assemblers.EAS.Reducer
import           HelVM.HelPA.Assemblers.EAS.TestData

import           HelVM.Expectations

import           Test.Hspec                          (Spec, describe, it)

spec :: Spec
spec = do
  describe "reduce" $ do
    forM_ [ ("true"     , trueIL           , trueIL)
          , ("hello"    , helloIL          , helloIL)
          , ("pip"      , pipIL            , pipILReduced)
          , ("pip2"     , pip2IL           , pip2ILReduced)
          , ("reverse"  , reverseIL        , reverseILReduced)
          , ("function" , functionIL       , functionIL)
          , ("add"      , addILLinked      , addILReduced)
          , ("writestr" , writeStrIL       , writeStrILReduced)
          , ("hello2"   , hello2ILLinked   , hello2ILReduced)
          , ("hello4"   , hello4ILLinked   , hello2ILReduced)
          , ("writenum" , writeNumILLinked , writeNumILReduced)
          , ("multiply" , multiplyIL       , multiplyILReduced)
          , ("readnum"  , readNumILLinked  , readNumILReduced)
          , ("fact"     , factILLinked     , factILReduced)
          , ("bottles"  , bottlesILLinked  , bottlesILReduced)
          , ("euclid"   , euclidIL         , euclidILReduced)
          ] $ \(fileName , ilLinked , ilReduced) -> do
      it fileName $ do reduce ilLinked `shouldSafe` ilReduced
