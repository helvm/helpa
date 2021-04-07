module HelVM.HelPA.Assemblers.WSA.Reducer where

import HelVM.HelPA.Assemblers.WSA.Instruction

import HelVM.HelPA.Common.Value

reduce :: InstructionList -> InstructionList
reduce il = reduceInstruction =<< il

reduceInstruction :: Instruction -> InstructionList
reduceInstruction (PushS (Literal [])) = [Push (Literal 0)]
reduceInstruction (PushS (Literal (x:xs))) = reduceInstruction (PushS (Literal xs)) <> [Push (Literal (fromIntegral (ord x)))]

reduceInstruction (Test v)            = [Dup, Push (Literal v), Sub Nothing]
reduceInstruction (Load (Just v)) = [Push v, Load Nothing]
reduceInstruction (Store   (Just v))  = [Push v, Swap, Store Nothing]

reduceInstruction (Add (Just v)) = [Push v, Add Nothing]
reduceInstruction (Sub (Just v)) = [Push v, Sub Nothing]
reduceInstruction (Mul (Just v)) = [Push v, Mul Nothing]
reduceInstruction (Div (Just v)) = [Push v, Div Nothing]
reduceInstruction (Mod (Just v)) = [Push v, Mod Nothing]

reduceInstruction (BranchPN l) = [BranchZ l1, Branch l, Mark l1] where
  l1 = calculateLocalLabel l 1

reduceInstruction (BranchNP l) = [BranchZ l1, Branch l, Mark l1] where
  l1 = calculateLocalLabel l 1

reduceInstruction (BranchPZ l) = [BranchN l1, Branch l, Mark l1] where
  l1 = calculateLocalLabel l 1

reduceInstruction (BranchP l) = [Dup, BranchN l1, Dup, BranchZ l1, Pop, Branch l, Mark l1, Pop] where
  l1 = calculateLocalLabel l 1
    
reduceInstruction (BranchNZ l) = [Dup, BranchN l1, Dup, BranchZ l1, Branch l2, Mark l1, Pop, Branch l, Mark l2, Pop] where
  l1 = calculateLocalLabel l 1
  l2 = calculateLocalLabel l 2

reduceInstruction i = [i]
      
calculateLocalLabel :: String -> Integer -> String
calculateLocalLabel label suffix = label <> ":" <> show suffix
