module HelVM.HelPA.Assembler.API.SwitchEnum where

import qualified Relude.Extra as Extra

bothEnums :: (Bounded e , Enum e) => [e]
bothEnums = enumFromBool <$> [False , True]

defaultEnum :: (Bounded e , Enum e) => e
defaultEnum = enumFromBool False

enumFromBool :: (Bounded e , Enum e) => Bool -> e
enumFromBool = unsafeEnum . fromEnum

unsafeEnum :: (Bounded e , Enum e) => Int -> e
unsafeEnum i = (unsafe . Extra.safeToEnum) i where
    unsafe Nothing  = error $ show i
    unsafe (Just a) = a
