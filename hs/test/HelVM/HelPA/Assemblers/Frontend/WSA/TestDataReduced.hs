module HelVM.HelPA.Assemblers.Frontend.WSA.TestDataReduced where

import           HelVM.HelPA.Assemblers.Backend.WSA.Instruction
import           HelVM.HelPA.Assemblers.Backend.WSA.Token

import           HelVM.HelPA.Assembler.Value


ioILReduced :: InstructionList
ioILReduced =
  [Mark "prints",Dup,BranchZ "prints_end",OutputChar,Branch "prints"
  ,Mark "prints_end",Pop,Return
  ,Mark "printsln",Call "prints",Push (Literal 10),OutputChar,Return
  ]

ioTL :: TokenList
ioTL = [N,S,S,S,T,T,T,S,S,S,S,S,T,T,T,S,S,T,S,S,T,T,S,T,S,S,T,S,T,T,S,T,T,T,S,S,T,T,T,S,T,S,S,S,T,T,T,S,S,T,T,N,S,N,S,N,T,S,S,T,T,T,S,S,S,S,S,T,T,T,S,S,T,S,S,T,T,S,T,S,S,T,S,T,T,S,T,T,T,S,S,T,T,T,S,T,S,S,S,T,T,T,S,S,T,T,S,T,S,T,T,T,T,T,S,T,T,S,S,T,S,T,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,S,N,T,N,S,S,N,S,N,S,T,T,T,S,S,S,S,S,T,T,T,S,S,T,S,S,T,T,S,T,S,S,T,S,T,T,S,T,T,T,S,S,T,T,T,S,T,S,S,S,T,T,T,S,S,T,T,N,N,S,S,S,T,T,T,S,S,S,S,S,T,T,T,S,S,T,S,S,T,T,S,T,S,S,T,S,T,T,S,T,T,T,S,S,T,T,T,S,T,S,S,S,T,T,T,S,S,T,T,S,T,S,T,T,T,T,T,S,T,T,S,S,T,S,T,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,S,N,S,S,N,N,T,N,N,S,S,S,T,T,T,S,S,S,S,S,T,T,T,S,S,T,S,S,T,T,S,T,S,S,T,S,T,T,S,T,T,T,S,S,T,T,T,S,T,S,S,S,T,T,T,S,S,T,T,S,T,T,S,T,T,S,S,S,T,T,S,T,T,T,S,N,N,S,T,S,T,T,T,S,S,S,S,S,T,T,T,S,S,T,S,S,T,T,S,T,S,S,T,S,T,T,S,T,T,T,S,S,T,T,T,S,T,S,S,S,T,T,T,S,S,T,T,N,S,S,S,T,S,T,S,T,N,S,S,N,T,N]

ioWS :: String
ioWS = ""

----

memoryILReduced :: InstructionList
memoryILReduced =
  [Mark "memcopy"
  ,Mark "mem_copy"
  ,Mark "mem_move",Push (Literal 3),Swap,Store,Push (Literal 2),Swap,Store,Push (Literal 1),Swap,Store,Push (Literal 3),Load,Dup,BranchM "memcopy_end:1",Dup,BranchZ "memcopy_end:1",Branch "memcopy_end:2"
  ,Mark "memcopy_end:1",Pop,Branch "memcopy_end"
  ,Mark "memcopy_end:2",Pop,Push (Literal 1),Load,Push (Literal 2),Load,Sub,BranchZ "memcopy_end",Push (Literal 1),Load,Push (Literal 2),Load,Sub,BranchM "memcopy_loop_dest_greater_source_begin",Branch "memcopy_loop_source_greater_dest"
  ,Mark "memcopy_loop_source_greater_dest",Push (Literal 2),Load,Push (Literal 1),Load,Load,Store,Push (Literal 2),Push (Literal 2),Load,Push (Literal 1),Add,Store,Push (Literal 1),Push (Literal 1),Load,Push (Literal 1),Add,Store,Push (Literal 3),Push (Literal 3),Load,Push (Literal 1),Sub,Store,Push (Literal 3),Load,BranchZ "memcopy_end",Branch "memcopy_loop_source_greater_dest"
  ,Mark "memcopy_loop_dest_greater_source_begin",Push (Literal 2),Push (Literal 2),Load,Push (Literal 3),Load,Add,Push (Literal 1),Sub,Store,Push (Literal 1),Push (Literal 1),Load,Push (Literal 3),Load,Add,Push (Literal 1),Sub,Store
  ,Mark "memcopy_loop_dest_greater_source",Push (Literal 2),Load,Push (Literal 1),Load,Load,Store,Push (Literal 2),Push (Literal 2),Load,Push (Literal 1),Sub,Store,Push (Literal 1),Push (Literal 1),Load,Push (Literal 1),Sub,Store,Push (Literal 3),Push (Literal 3),Load,Push (Literal 1),Sub,Store,Push (Literal 3),Load,BranchZ "memcopy_end",Branch "memcopy_loop_dest_greater_source"
  ,Mark "memcopy_end",Return
  ,Mark "mem_zero"
  ,Mark "mem_zero_start",Dup,BranchZ "mem_zero_end",Swap,Dup,Push (Literal 0),Store,Push (Literal 1),Add,Swap,Push (Literal 1),Sub,Branch "mem_zero_start"
  ,Mark "mem_zero_end",Pop,Pop,Return
  ,Mark "numeriere",Push (Literal 2),Swap,Store,Push (Literal 1),Swap,Store,Push (Literal 1),Load
  ,Mark "numeriere_start",Dup,Dup,Store,Push (Literal 1),Add,Dup,Push (Literal 2),Load,Sub,BranchZ "numeriere_end",Branch "numeriere_start"
  ,Mark "numeriere_end",Pop,Return]

memoryTL :: TokenList
memoryTL =  [N,S,S,S,T,T,S,T,T,S,T,S,T,T,S,S,T,S,T,S,T,T,S,T,T,S,T,S,T,T,S,S,S,T,T,S,T,T,S,T,T,T,T,S,T,T,T,S,S,S,S,S,T,T,T,T,S,S,T,N,N,S,S,S,T,T,S,T,T,S,T,S,T,T,S,S,T,S,T,S,T,T,S,T,T,S,T,S,T,S,T,T,T,T,T,S,T,T,S,S,S,T,T,S,T,T,S,T,T,T,T,S,T,T,T,S,S,S,S,S,T,T,T,T,S,S,T,N,N,S,S,S,T,T,S,T,T,S,T,S,T,T,S,S,T,S,T,S,T,T,S,T,T,S,T,S,T,S,T,T,T,T,T,S,T,T,S,T,T,S,T,S,T,T,S,T,T,T,T,S,T,T,T,S,T,T,S,S,T,T,S,S,T,S,T,N,S,S,S,T,T,S,N,T,T,T,S,S,S,S,T,S,S,N,T,T,T,S,S,S,S,T,S,N,T,T,T,S,S,S,S,T,T,T,T,T,S,N,S,N,T,T,S,T,T,S,T,T,S,T,S,T,T,S,S,T,S,T,S,T,T,S,T,T,S,T,S,T,T,S,S,S,T,T,S,T,T,S,T,T,T,T,S,T,T,T,S,S,S,S,S,T,T,T,T,S,S,T,S,T,S,T,T,T,T,T,S,T,T,S,S,T,S,T,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,S,S,S,T,T,T,S,T,S,S,S,T,T,S,S,S,T,N,S,N,S,N,T,S,S,T,T,S,T,T,S,T,S,T,T,S,S,T,S,T,S,T,T,S,T,T,S,T,S,T,T,S,S,S,T,T,S,T,T,S,T,T,T,T,S,T,T,T,S,S,S,S,S,T,T,T,T,S,S,T,S,T,S,T,T,T,T,T,S,T,T,S,S,T,S,T,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,S,S,S,T,T,T,S,T,S,S,S,T,T,S,S,S,T,N,N,S,N,S,T,T,S,T,T,S,T,S,T,T,S,S,T,S,T,S,T,T,S,T,T,S,T,S,T,T,S,S,S,T,T,S,T,T,S,T,T,T,T,S,T,T,T,S,S,S,S,S,T,T,T,T,S,S,T,S,T,S,T,T,T,T,T,S,T,T,S,S,T,S,T,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,S,S,S,T,T,T,S,T,S,S,S,T,T,S,S,T,S,N,N,S,S,S,T,T,S,T,T,S,T,S,T,T,S,S,T,S,T,S,T,T,S,T,T,S,T,S,T,T,S,S,S,T,T,S,T,T,S,T,T,T,T,S,T,T,T,S,S,S,S,S,T,T,T,T,S,S,T,S,T,S,T,T,T,T,T,S,T,T,S,S,T,S,T,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,S,S,S,T,T,T,S,T,S,S,S,T,T,S,S,S,T,N,S,S,N,N,S,N,S,T,T,S,T,T,S,T,S,T,T,S,S,T,S,T,S,T,T,S,T,T,S,T,S,T,T,S,S,S,T,T,S,T,T,S,T,T,T,T,S,T,T,T,S,S,S,S,S,T,T,T,T,S,S,T,S,T,S,T,T,T,T,T,S,T,T,S,S,T,S,T,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,S,N,N,S,S,S,T,T,S,T,T,S,T,S,T,T,S,S,T,S,T,S,T,T,S,T,T,S,T,S,T,T,S,S,S,T,T,S,T,T,S,T,T,T,T,S,T,T,T,S,S,S,S,S,T,T,T,T,S,S,T,S,T,S,T,T,T,T,T,S,T,T,S,S,T,S,T,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,S,S,S,T,T,T,S,T,S,S,S,T,T,S,S,T,S,N,S,S,N,S,S,S,T,T,T,T,S,S,S,T,S,T,T,T,T,S,S,T,N,T,S,S,T,T,S,T,T,S,T,S,T,T,S,S,T,S,T,S,T,T,S,T,T,S,T,S,T,T,S,S,S,T,T,S,T,T,S,T,T,T,T,S,T,T,T,S,S,S,S,S,T,T,T,T,S,S,T,S,T,S,T,T,T,T,T,S,T,T,S,S,T,S,T,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,S,N,S,S,S,T,T,T,T,S,S,S,T,S,T,T,T,T,S,S,T,N,T,T,S,T,T,S,T,T,S,T,S,T,T,S,S,T,S,T,S,T,T,S,T,T,S,T,S,T,T,S,S,S,T,T,S,T,T,S,T,T,T,T,S,T,T,T,S,S,S,S,S,T,T,T,T,S,S,T,S,T,S,T,T,T,T,T,S,T,T,S,T,T,S,S,S,T,T,S,T,T,T,T,S,T,T,S,T,T,T,T,S,T,T,T,S,S,S,S,S,T,S,T,T,T,T,T,S,T,T,S,S,T,S,S,S,T,T,S,S,T,S,T,S,T,T,T,S,S,T,T,S,T,T,T,S,T,S,S,S,T,S,T,T,T,T,T,S,T,T,S,S,T,T,T,S,T,T,T,S,S,T,S,S,T,T,S,S,T,S,T,S,T,T,S,S,S,S,T,S,T,T,T,S,T,S,S,S,T,T,S,S,T,S,T,S,T,T,T,S,S,T,S,S,T,S,T,T,T,T,T,S,T,T,T,S,S,T,T,S,T,T,S,T,T,T,T,S,T,T,T,S,T,S,T,S,T,T,T,S,S,T,S,S,T,T,S,S,S,T,T,S,T,T,S,S,T,S,T,S,T,S,T,T,T,T,T,S,T,T,S,S,S,T,S,S,T,T,S,S,T,S,T,S,T,T,S,S,T,T,T,S,T,T,S,T,S,S,T,S,T,T,S,T,T,T,S,N,N,S,N,S,T,T,S,T,T,S,T,S,T,T,S,S,T,S,T,S,T,T,S,T,T,S,T,S,T,T,S,S,S,T,T,S,T,T,S,T,T,T,T,S,T,T,T,S,S,S,S,S,T,T,T,T,S,S,T,S,T,S,T,T,T,T,T,S,T,T,S,T,T,S,S,S,T,T,S,T,T,T,T,S,T,T,S,T,T,T,T,S,T,T,T,S,S,S,S,S,T,S,T,T,T,T,T,S,T,T,T,S,S,T,T,S,T,T,S,T,T,T,T,S,T,T,T,S,T,S,T,S,T,T,T,S,S,T,S,S,T,T,S,S,S,T,T,S,T,T,S,S,T,S,T,S,T,S,T,T,T,T,T,S,T,T,S,S,T,T,T,S,T,T,T,S,S,T,S,S,T,T,S,S,T,S,T,S,T,T,S,S,S,S,T,S,T,T,T,S,T,S,S,S,T,T,S,S,T,S,T,S,T,T,T,S,S,T,S,S,T,S,T,T,T,T,T,S,T,T,S,S,T,S,S,S,T,T,S,S,T,S,T,S,T,T,T,S,S,T,T,S,T,T,T,S,T,S,S,N,N,S,S,S,T,T,S,T,T,S,T,S,T,T,S,S,T,S,T,S,T,T,S,T,T,S,T,S,T,T,S,S,S,T,T,S,T,T,S,T,T,T,T,S,T,T,T,S,S,S,S,S,T,T,T,T,S,S,T,S,T,S,T,T,T,T,T,S,T,T,S,T,T,S,S,S,T,T,S,T,T,T,T,S,T,T,S,T,T,T,T,S,T,T,T,S,S,S,S,S,T,S,T,T,T,T,T,S,T,T,T,S,S,T,T,S,T,T,S,T,T,T,T,S,T,T,T,S,T,S,T,S,T,T,T,S,S,T,S,S,T,T,S,S,S,T,T,S,T,T,S,S,T,S,T,S,T,S,T,T,T,T,T,S,T,T,S,S,T,T,T,S,T,T,T,S,S,T,S,S,T,T,S,S,T,S,T,S,T,T,S,S,S,S,T,S,T,T,T,S,T,S,S,S,T,T,S,S,T,S,T,S,T,T,T,S,S,T,S,S,T,S,T,T,T,T,T,S,T,T,S,S,T,S,S,S,T,T,S,S,T,S,T,S,T,T,T,S,S,T,T,S,T,T,T,S,T,S,S,N,S,S,S,T,S,T,T,T,S,S,S,T,T,T,T,T,T,T,T,T,S,S,S,S,T,S,S,S,S,T,S,T,T,T,S,S,S,T,T,S,S,S,T,T,S,S,S,S,T,S,S,S,T,T,T,T,S,S,S,T,T,S,S,S,T,T,S,S,S,S,T,T,S,S,S,T,T,T,T,T,S,S,S,T,T,S,S,T,T,T,S,S,S,S,T,T,T,T,T,N,T,S,S,T,T,S,T,T,S,T,S,T,T,S,S,T,S,T,S,T,T,S,T,T,S,T,S,T,T,S,S,S,T,T,S,T,T,S,T,T,T,T,S,T,T,T,S,S,S,S,S,T,T,T,T,S,S,T,S,T,S,T,T,T,T,T,S,T,T,S,S,T,S,T,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,S,N,N,S,N,S,T,T,S,T,T,S,T,S,T,T,S,S,T,S,T,S,T,T,S,T,T,S,T,S,T,T,S,S,S,T,T,S,T,T,S,T,T,T,T,S,T,T,T,S,S,S,S,S,T,T,T,T,S,S,T,S,T,S,T,T,T,T,T,S,T,T,S,T,T,S,S,S,T,T,S,T,T,T,T,S,T,T,S,T,T,T,T,S,T,T,T,S,S,S,S,S,T,S,T,T,T,T,T,S,T,T,T,S,S,T,T,S,T,T,S,T,T,T,T,S,T,T,T,S,T,S,T,S,T,T,T,S,S,T,S,S,T,T,S,S,S,T,T,S,T,T,S,S,T,S,T,S,T,S,T,T,T,T,T,S,T,T,S,S,T,T,T,S,T,T,T,S,S,T,S,S,T,T,S,S,T,S,T,S,T,T,S,S,S,S,T,S,T,T,T,S,T,S,S,S,T,T,S,S,T,S,T,S,T,T,T,S,S,T,S,S,T,S,T,T,T,T,T,S,T,T,S,S,T,S,S,S,T,T,S,S,T,S,T,S,T,T,T,S,S,T,T,S,T,T,T,S,T,S,S,N,N,S,S,S,T,T,S,T,T,S,T,S,T,T,S,S,T,S,T,S,T,T,S,T,T,S,T,S,T,T,S,S,S,T,T,S,T,T,S,T,T,T,T,S,T,T,T,S,S,S,S,S,T,T,T,T,S,S,T,S,T,S,T,T,T,T,T,S,T,T,S,T,T,S,S,S,T,T,S,T,T,T,T,S,T,T,S,T,T,T,T,S,T,T,T,S,S,S,S,S,T,S,T,T,T,T,T,S,T,T,S,S,T,S,S,S,T,T,S,S,T,S,T,S,T,T,T,S,S,T,T,S,T,T,T,S,T,S,S,S,T,S,T,T,T,T,T,S,T,T,S,S,T,T,T,S,T,T,T,S,S,T,S,S,T,T,S,S,T,S,T,S,T,T,S,S,S,S,T,S,T,T,T,S,T,S,S,S,T,T,S,S,T,S,T,S,T,T,T,S,S,T,S,S,T,S,T,T,T,T,T,S,T,T,T,S,S,T,T,S,T,T,S,T,T,T,T,S,T,T,T,S,T,S,T,S,T,T,T,S,S,T,S,S,T,T,S,S,S,T,T,S,T,T,S,S,T,S,T,S,T,S,T,T,T,T,T,S,T,T,S,S,S,T,S,S,T,T,S,S,T,S,T,S,T,T,S,S,T,T,T,S,T,T,S,T,S,S,T,S,T,T,S,T,T,T,S,N,S,S,S,T,S,S,S,S,T,S,T,T,T,S,S,S,T,T,T,T,T,T,S,S,S,S,S,S,T,T,S,S,T,T,T,S,S,S,S,T,S,S,S,T,T,T,T,S,S,S,T,T,T,T,T,T,S,S,S,S,S,S,T,T,S,S,T,T,T,S,N,S,S,S,T,T,S,T,T,S,T,S,T,T,S,S,T,S,T,S,T,T,S,T,T,S,T,S,T,T,S,S,S,T,T,S,T,T,S,T,T,T,T,S,T,T,T,S,S,S,S,S,T,T,T,T,S,S,T,S,T,S,T,T,T,T,T,S,T,T,S,T,T,S,S,S,T,T,S,T,T,T,T,S,T,T,S,T,T,T,T,S,T,T,T,S,S,S,S,S,T,S,T,T,T,T,T,S,T,T,S,S,T,S,S,S,T,T,S,S,T,S,T,S,T,T,T,S,S,T,T,S,T,T,T,S,T,S,S,S,T,S,T,T,T,T,T,S,T,T,S,S,T,T,T,S,T,T,T,S,S,T,S,S,T,T,S,S,T,S,T,S,T,T,S,S,S,S,T,S,T,T,T,S,T,S,S,S,T,T,S,S,T,S,T,S,T,T,T,S,S,T,S,S,T,S,T,T,T,T,T,S,T,T,T,S,S,T,T,S,T,T,S,T,T,T,T,S,T,T,T,S,T,S,T,S,T,T,T,S,S,T,S,S,T,T,S,S,S,T,T,S,T,T,S,S,T,S,T,N,S,S,S,T,S,T,T,T,S,S,S,T,T,T,T,T,T,T,T,T,S,S,S,S,T,S,S,S,S,T,S,T,T,T,S,S,S,T,T,S,S,T,T,T,S,S,S,S,T,S,S,S,T,T,T,T,S,S,S,T,T,S,S,T,T,T,S,S,S,S,T,T,S,S,S,T,T,T,T,T,S,S,S,T,T,S,S,T,T,T,S,S,S,S,T,T,T,T,T,N,T,S,S,T,T,S,T,T,S,T,S,T,T,S,S,T,S,T,S,T,T,S,T,T,S,T,S,T,T,S,S,S,T,T,S,T,T,S,T,T,T,T,S,T,T,T,S,S,S,S,S,T,T,T,T,S,S,T,S,T,S,T,T,T,T,T,S,T,T,S,S,T,S,T,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,S,N,N,S,N,S,T,T,S,T,T,S,T,S,T,T,S,S,T,S,T,S,T,T,S,T,T,S,T,S,T,T,S,S,S,T,T,S,T,T,S,T,T,T,T,S,T,T,T,S,S,S,S,S,T,T,T,T,S,S,T,S,T,S,T,T,T,T,T,S,T,T,S,T,T,S,S,S,T,T,S,T,T,T,T,S,T,T,S,T,T,T,T,S,T,T,T,S,S,S,S,S,T,S,T,T,T,T,T,S,T,T,S,S,T,S,S,S,T,T,S,S,T,S,T,S,T,T,T,S,S,T,T,S,T,T,T,S,T,S,S,S,T,S,T,T,T,T,T,S,T,T,S,S,T,T,T,S,T,T,T,S,S,T,S,S,T,T,S,S,T,S,T,S,T,T,S,S,S,S,T,S,T,T,T,S,T,S,S,S,T,T,S,S,T,S,T,S,T,T,T,S,S,T,S,S,T,S,T,T,T,T,T,S,T,T,T,S,S,T,T,S,T,T,S,T,T,T,T,S,T,T,T,S,T,S,T,S,T,T,T,S,S,T,S,S,T,T,S,S,S,T,T,S,T,T,S,S,T,S,T,N,N,S,S,S,T,T,S,T,T,S,T,S,T,T,S,S,T,S,T,S,T,T,S,T,T,S,T,S,T,T,S,S,S,T,T,S,T,T,S,T,T,T,T,S,T,T,T,S,S,S,S,S,T,T,T,T,S,S,T,S,T,S,T,T,T,T,T,S,T,T,S,S,T,S,T,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,S,N,N,T,N,N,S,S,S,T,T,S,T,T,S,T,S,T,T,S,S,T,S,T,S,T,T,S,T,T,S,T,S,T,S,T,T,T,T,T,S,T,T,T,T,S,T,S,S,T,T,S,S,T,S,T,S,T,T,T,S,S,T,S,S,T,T,S,T,T,T,T,N,N,S,S,S,T,T,S,T,T,S,T,S,T,T,S,S,T,S,T,S,T,T,S,T,T,S,T,S,T,S,T,T,T,T,T,S,T,T,T,T,S,T,S,S,T,T,S,S,T,S,T,S,T,T,T,S,S,T,S,S,T,T,S,T,T,T,T,S,T,S,T,T,T,T,T,S,T,T,T,S,S,T,T,S,T,T,T,S,T,S,S,S,T,T,S,S,S,S,T,S,T,T,T,S,S,T,S,S,T,T,T,S,T,S,S,N,S,N,S,N,T,S,S,T,T,S,T,T,S,T,S,T,T,S,S,T,S,T,S,T,T,S,T,T,S,T,S,T,S,T,T,T,T,T,S,T,T,T,T,S,T,S,S,T,T,S,S,T,S,T,S,T,T,T,S,S,T,S,S,T,T,S,T,T,T,T,S,T,S,T,T,T,T,T,S,T,T,S,S,T,S,T,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,S,N,S,N,T,S,N,S,S,S,S,T,T,S,S,S,S,T,T,S,S,S,S,N,T,S,S,S,T,T,S,S,T,N,S,N,S,T,T,S,T,T,S,T,S,T,T,S,S,T,S,T,S,T,T,S,T,T,S,T,S,T,S,T,T,T,T,T,S,T,T,T,T,S,T,S,S,T,T,S,S,T,S,T,S,T,T,T,S,S,T,S,S,T,T,S,T,T,T,T,S,T,S,T,T,T,T,T,S,T,T,T,S,S,T,T,S,T,T,T,S,T,S,S,S,T,T,S,S,S,S,T,S,T,T,T,S,S,T,S,S,T,T,T,S,T,S,S,N,N,S,S,S,T,T,S,T,T,S,T,S,T,T,S,S,T,S,T,S,T,T,S,T,T,S,T,S,T,S,T,T,T,T,T,S,T,T,T,T,S,T,S,S,T,T,S,S,T,S,T,S,T,T,T,S,S,T,S,S,T,T,S,T,T,T,T,S,T,S,T,T,T,T,T,S,T,T,S,S,T,S,T,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,S,N,S,S,N,S,S,N,N,T,N,N,S,S,S,T,T,S,T,T,T,S,S,T,T,T,S,T,S,T,S,T,T,S,T,T,S,T,S,T,T,S,S,T,S,T,S,T,T,T,S,S,T,S,S,T,T,S,T,S,S,T,S,T,T,S,S,T,S,T,S,T,T,T,S,S,T,S,S,T,T,S,S,T,S,T,N,S,S,S,T,S,S,N,T,T,T,S,S,S,S,T,S,N,T,T,T,S,S,S,S,T,T,T,T,N,S,S,S,T,T,S,T,T,T,S,S,T,T,T,S,T,S,T,S,T,T,S,T,T,S,T,S,T,T,S,S,T,S,T,S,T,T,T,S,S,T,S,S,T,T,S,T,S,S,T,S,T,T,S,S,T,S,T,S,T,T,T,S,S,T,S,S,T,T,S,S,T,S,T,S,T,S,T,T,T,T,T,S,T,T,T,S,S,T,T,S,T,T,T,S,T,S,S,S,T,T,S,S,S,S,T,S,T,T,T,S,S,T,S,S,T,T,T,S,T,S,S,N,S,N,S,S,N,S,T,T,S,S,S,S,T,T,S,S,S,S,N,S,S,S,S,T,S,T,T,T,T,S,S,T,N,T,S,S,T,T,S,T,T,T,S,S,T,T,T,S,T,S,T,S,T,T,S,T,T,S,T,S,T,T,S,S,T,S,T,S,T,T,T,S,S,T,S,S,T,T,S,T,S,S,T,S,T,T,S,S,T,S,T,S,T,T,T,S,S,T,S,S,T,T,S,S,T,S,T,S,T,S,T,T,T,T,T,S,T,T,S,S,T,S,T,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,S,N,N,S,N,S,T,T,S,T,T,T,S,S,T,T,T,S,T,S,T,S,T,T,S,T,T,S,T,S,T,T,S,S,T,S,T,S,T,T,T,S,S,T,S,S,T,T,S,T,S,S,T,S,T,T,S,S,T,S,T,S,T,T,T,S,S,T,S,S,T,T,S,S,T,S,T,S,T,S,T,T,T,T,T,S,T,T,T,S,S,T,T,S,T,T,T,S,T,S,S,S,T,T,S,S,S,S,T,S,T,T,T,S,S,T,S,S,T,T,T,S,T,S,S,N,N,S,S,S,T,T,S,T,T,T,S,S,T,T,T,S,T,S,T,S,T,T,S,T,T,S,T,S,T,T,S,S,T,S,T,S,T,T,T,S,S,T,S,S,T,T,S,T,S,S,T,S,T,T,S,S,T,S,T,S,T,T,T,S,S,T,S,S,T,T,S,S,T,S,T,S,T,S,T,T,T,T,T,S,T,T,S,S,T,S,T,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,S,N,S,S,N,N,T,N]

memoryWS :: String
memoryWS = ""

----


primILReduced :: InstructionList
primILReduced =
  [Mark "st",Push (Literal 0),Push (Literal 93),Push (Literal 48),Push (Literal 48),Push (Literal 49),Push (Literal 45),Push (Literal 50),Push (Literal 91),Push (Literal 32),Push (Literal 115),Push (Literal 114),Push (Literal 101),Push (Literal 98),Push (Literal 109),Push (Literal 117),Push (Literal 110),Push (Literal 32),Push (Literal 109),Push (Literal 105),Push (Literal 114),Push (Literal 112),Call "printsln",Push (Literal 0)
  ,Mark "f_start_1",Dup,Push (Literal 100),Sub,BranchZ "f_finished_1",Dup,Push (Literal 100),Add,Push (Literal 1),Store,Push (Literal 1),Add,Branch "f_start_1"
  ,Mark "f_finished_1",Pop,Push (Literal 2)
  ,Mark "f_start_2",Dup,Push (Literal 11),Sub,BranchZ "f_finished_2",Push (Literal 2)
  ,Mark "f_start_3",Dup,Push (Literal 100),Sub,BranchZ "f_finished_3"
  ,Dup,Push (Literal 1),Swap,Store,Swap,Dup,Push (Literal 2),Swap,Store,Swap,Push (Literal 1),Load,Push (Literal 2),Load,Mul,Push (Literal 100),Add,Push (Literal 0),Store,Push (Literal 1),Add,Branch "f_start_3"
  ,Mark "f_finished_3",Pop,Push (Literal 1),Add,Branch "f_start_2"
  ,Mark "f_finished_2",Pop,Push (Literal 2)
  ,Mark "f_start_4",Dup,Push (Literal 100),Sub,BranchZ "f_finished_4",Dup,Push (Literal 100),Add,Load,BranchZ "nodraw",Dup,OutputNum,Push (Literal 0),Push (Literal 32),Call "prints"
  ,Mark "nodraw",Push (Literal 1),Add,Branch "f_start_4"
  ,Mark "f_finished_4",Pop,Push (Literal 0),Call "printsln",End]

primTL :: TokenList
primTL = [N,S,S,S,T,T,T,S,S,T,T,S,T,T,T,S,T,S,S,N,S,S,S,S,S,S,T,S,T,T,T,S,T,S,S,S,T,T,S,S,S,S,S,S,S,T,T,S,S,S,S,S,S,S,T,T,S,S,S,T,S,S,S,T,S,T,T,S,T,S,S,S,T,T,S,S,T,S,S,S,S,T,S,T,T,S,T,T,S,S,S,T,S,S,S,S,S,S,S,S,T,T,T,S,S,T,T,S,S,S,T,T,T,S,S,T,S,S,S,S,T,T,S,S,T,S,T,S,S,S,T,T,S,S,S,T,S,S,S,S,T,T,S,T,T,S,T,S,S,S,T,T,T,S,T,S,T,S,S,S,T,T,S,T,T,T,S,S,S,S,T,S,S,S,S,S,S,S,S,T,T,S,T,T,S,T,S,S,S,T,T,S,T,S,S,T,S,S,S,T,T,T,S,S,T,S,S,S,S,T,T,T,S,S,S,S,N,S,T,S,T,T,T,S,S,S,S,S,T,T,T,S,S,T,S,S,T,T,S,T,S,S,T,S,T,T,S,T,T,T,S,S,T,T,T,S,T,S,S,S,T,T,T,S,S,T,T,S,T,T,S,T,T,S,S,S,T,T,S,T,T,T,S,N,S,S,S,N,S,S,S,T,T,S,S,T,T,S,S,T,S,T,T,T,T,T,S,T,T,T,S,S,T,T,S,T,T,T,S,T,S,S,S,T,T,S,S,S,S,T,S,T,T,T,S,S,T,S,S,T,T,T,S,T,S,S,S,T,S,T,T,T,T,T,S,S,T,T,S,S,S,T,N,S,N,S,S,S,S,T,T,S,S,T,S,S,T,S,S,T,N,T,S,S,T,T,S,S,T,T,S,S,T,S,T,T,T,T,T,S,T,T,S,S,T,T,S,S,T,T,S,T,S,S,T,S,T,T,S,T,T,T,S,S,T,T,S,T,S,S,T,S,T,T,T,S,S,T,T,S,T,T,S,T,S,S,S,S,T,T,S,S,T,S,T,S,T,T,S,S,T,S,S,S,T,S,T,T,T,T,T,S,S,T,T,S,S,S,T,N,S,N,S,S,S,S,T,T,S,S,T,S,S,T,S,S,S,S,S,S,T,T,T,S,S,S,S,T,T,S,S,S,N,S,N,S,T,T,S,S,T,T,S,S,T,S,T,T,T,T,T,S,T,T,T,S,S,T,T,S,T,T,T,S,T,S,S,S,T,T,S,S,S,S,T,S,T,T,T,S,S,T,S,S,T,T,T,S,T,S,S,S,T,S,T,T,T,T,T,S,S,T,T,S,S,S,T,N,N,S,S,S,T,T,S,S,T,T,S,S,T,S,T,T,T,T,T,S,T,T,S,S,T,T,S,S,T,T,S,T,S,S,T,S,T,T,S,T,T,T,S,S,T,T,S,T,S,S,T,S,T,T,T,S,S,T,T,S,T,T,S,T,S,S,S,S,T,T,S,S,T,S,T,S,T,T,S,S,T,S,S,S,T,S,T,T,T,T,T,S,S,T,T,S,S,S,T,N,S,S,N,S,S,S,T,S,N,S,S,S,T,T,S,S,T,T,S,S,T,S,T,T,T,T,T,S,T,T,T,S,S,T,T,S,T,T,T,S,T,S,S,S,T,T,S,S,S,S,T,S,T,T,T,S,S,T,S,S,T,T,T,S,T,S,S,S,T,S,T,T,T,T,T,S,S,T,T,S,S,T,S,N,S,N,S,S,S,S,T,S,T,T,T,S,S,T,N,T,S,S,T,T,S,S,T,T,S,S,T,S,T,T,T,T,T,S,T,T,S,S,T,T,S,S,T,T,S,T,S,S,T,S,T,T,S,T,T,T,S,S,T,T,S,T,S,S,T,S,T,T,T,S,S,T,T,S,T,T,S,T,S,S,S,S,T,T,S,S,T,S,T,S,T,T,S,S,T,S,S,S,T,S,T,T,T,T,T,S,S,T,T,S,S,T,S,N,S,S,S,T,S,N,S,S,S,T,T,S,S,T,T,S,S,T,S,T,T,T,T,T,S,T,T,T,S,S,T,T,S,T,T,T,S,T,S,S,S,T,T,S,S,S,S,T,S,T,T,T,S,S,T,S,S,T,T,T,S,T,S,S,S,T,S,T,T,T,T,T,S,S,T,T,S,S,T,T,N,S,N,S,S,S,S,T,T,S,S,T,S,S,T,S,S,T,N,T,S,S,T,T,S,S,T,T,S,S,T,S,T,T,T,T,T,S,T,T,S,S,T,T,S,S,T,T,S,T,S,S,T,S,T,T,S,T,T,T,S,S,T,T,S,T,S,S,T,S,T,T,T,S,S,T,T,S,T,T,S,T,S,S,S,S,T,T,S,S,T,S,T,S,T,T,S,S,T,S,S,S,T,S,T,T,T,T,T,S,S,T,T,S,S,T,T,N,S,N,S,S,S,S,T,S,N,T,T,T,S,S,N,T,S,N,S,S,S,S,T,S,S,N,T,T,T,S,S,N,T,S,S,S,T,T,T,T,S,S,S,T,S,T,T,T,T,S,S,N,S,S,S,T,T,S,S,T,S,S,T,S,S,S,S,S,S,T,T,S,S,S,S,T,T,S,S,S,N,S,N,S,T,T,S,S,T,T,S,S,T,S,T,T,T,T,T,S,T,T,T,S,S,T,T,S,T,T,T,S,T,S,S,S,T,T,S,S,S,S,T,S,T,T,T,S,S,T,S,S,T,T,T,S,T,S,S,S,T,S,T,T,T,T,T,S,S,T,T,S,S,T,T,N,N,S,S,S,T,T,S,S,T,T,S,S,T,S,T,T,T,T,T,S,T,T,S,S,T,T,S,S,T,T,S,T,S,S,T,S,T,T,S,T,T,T,S,S,T,T,S,T,S,S,T,S,T,T,T,S,S,T,T,S,T,T,S,T,S,S,S,S,T,T,S,S,T,S,T,S,T,T,S,S,T,S,S,S,T,S,T,T,T,T,T,S,S,T,T,S,S,T,T,N,S,S,N,S,S,S,T,T,S,S,S,N,S,N,S,T,T,S,S,T,T,S,S,T,S,T,T,T,T,T,S,T,T,T,S,S,T,T,S,T,T,T,S,T,S,S,S,T,T,S,S,S,S,T,S,T,T,T,S,S,T,S,S,T,T,T,S,T,S,S,S,T,S,T,T,T,T,T,S,S,T,T,S,S,T,S,N,N,S,S,S,T,T,S,S,T,T,S,S,T,S,T,T,T,T,T,S,T,T,S,S,T,T,S,S,T,T,S,T,S,S,T,S,T,T,S,T,T,T,S,S,T,T,S,T,S,S,T,S,T,T,T,S,S,T,T,S,T,T,S,T,S,S,S,S,T,T,S,S,T,S,T,S,T,T,S,S,T,S,S,S,T,S,T,T,T,T,T,S,S,T,T,S,S,T,S,N,S,S,N,S,S,S,T,S,N,S,S,S,T,T,S,S,T,T,S,S,T,S,T,T,T,T,T,S,T,T,T,S,S,T,T,S,T,T,T,S,T,S,S,S,T,T,S,S,S,S,T,S,T,T,T,S,S,T,S,S,T,T,T,S,T,S,S,S,T,S,T,T,T,T,T,S,S,T,T,S,T,S,S,N,S,N,S,S,S,S,T,T,S,S,T,S,S,T,S,S,T,N,T,S,S,T,T,S,S,T,T,S,S,T,S,T,T,T,T,T,S,T,T,S,S,T,T,S,S,T,T,S,T,S,S,T,S,T,T,S,T,T,T,S,S,T,T,S,T,S,S,T,S,T,T,T,S,S,T,T,S,T,T,S,T,S,S,S,S,T,T,S,S,T,S,T,S,T,T,S,S,T,S,S,S,T,S,T,T,T,T,T,S,S,T,T,S,T,S,S,N,S,N,S,S,S,S,T,T,S,S,T,S,S,T,S,S,S,T,T,T,N,T,S,S,T,T,S,T,T,T,S,S,T,T,S,T,T,T,T,S,T,T,S,S,T,S,S,S,T,T,T,S,S,T,S,S,T,T,S,S,S,S,T,S,T,T,T,S,T,T,T,N,S,N,S,T,N,S,T,S,S,S,S,S,S,T,S,S,S,S,S,N,S,T,S,T,T,T,S,S,S,S,S,T,T,T,S,S,T,S,S,T,T,S,T,S,S,T,S,T,T,S,T,T,T,S,S,T,T,T,S,T,S,S,S,T,T,T,S,S,T,T,N,N,S,S,S,T,T,S,T,T,T,S,S,T,T,S,T,T,T,T,S,T,T,S,S,T,S,S,S,T,T,T,S,S,T,S,S,T,T,S,S,S,S,T,S,T,T,T,S,T,T,T,N,S,S,S,T,T,S,S,S,N,S,N,S,T,T,S,S,T,T,S,S,T,S,T,T,T,T,T,S,T,T,T,S,S,T,T,S,T,T,T,S,T,S,S,S,T,T,S,S,S,S,T,S,T,T,T,S,S,T,S,S,T,T,T,S,T,S,S,S,T,S,T,T,T,T,T,S,S,T,T,S,T,S,S,N,N,S,S,S,T,T,S,S,T,T,S,S,T,S,T,T,T,T,T,S,T,T,S,S,T,T,S,S,T,T,S,T,S,S,T,S,T,T,S,T,T,T,S,S,T,T,S,T,S,S,T,S,T,T,T,S,S,T,T,S,T,T,S,T,S,S,S,S,T,T,S,S,T,S,T,S,T,T,S,S,T,S,S,S,T,S,T,T,T,T,T,S,S,T,T,S,T,S,S,N,S,S,N,S,S,S,N,S,T,S,T,T,T,S,S,S,S,S,T,T,T,S,S,T,S,S,T,T,S,T,S,S,T,S,T,T,S,T,T,T,S,S,T,T,T,S,T,S,S,S,T,T,T,S,S,T,T,S,T,T,S,T,T,S,S,S,T,T,S,T,T,T,S,N,N,N,N,N,S,S,S,T,T,T,S,S,S,S,S,T,T,T,S,S,T,S,S,T,T,S,T,S,S,T,S,T,T,S,T,T,T,S,S,T,T,T,S,T,S,S,S,T,T,T,S,S,T,T,N,S,N,S,N,T,S,S,T,T,T,S,S,S,S,S,T,T,T,S,S,T,S,S,T,T,S,T,S,S,T,S,T,T,S,T,T,T,S,S,T,T,T,S,T,S,S,S,T,T,T,S,S,T,T,S,T,S,T,T,T,T,T,S,T,T,S,S,T,S,T,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,S,N,T,N,S,S,N,S,N,S,T,T,T,S,S,S,S,S,T,T,T,S,S,T,S,S,T,T,S,T,S,S,T,S,T,T,S,T,T,T,S,S,T,T,T,S,T,S,S,S,T,T,T,S,S,T,T,N,N,S,S,S,T,T,T,S,S,S,S,S,T,T,T,S,S,T,S,S,T,T,S,T,S,S,T,S,T,T,S,T,T,T,S,S,T,T,T,S,T,S,S,S,T,T,T,S,S,T,T,S,T,S,T,T,T,T,T,S,T,T,S,S,T,S,T,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,S,N,S,S,N,N,T,N,N,S,S,S,T,T,T,S,S,S,S,S,T,T,T,S,S,T,S,S,T,T,S,T,S,S,T,S,T,T,S,T,T,T,S,S,T,T,T,S,T,S,S,S,T,T,T,S,S,T,T,S,T,T,S,T,T,S,S,S,T,T,S,T,T,T,S,N,N,S,T,S,T,T,T,S,S,S,S,S,T,T,T,S,S,T,S,S,T,T,S,T,S,S,T,S,T,T,S,T,T,T,S,S,T,T,T,S,T,S,S,S,T,T,T,S,S,T,T,N,S,S,S,T,S,T,S,T,N,S,S,N,T,N]

primWS :: String
primWS = ""
