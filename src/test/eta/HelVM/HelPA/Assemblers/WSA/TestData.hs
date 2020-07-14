module HelVM.HelPA.Assemblers.WSA.TestData where

import HelVM.HelPA.Assemblers.WSA.Instruction

io :: InstructionList
io =
  [Mark "prints",Doub,JumpZ "prints_end",OutC,Jump "prints"
  ,Mark "prints_end",Pop,Ret
  ,Mark "printsln",Call "prints",Push 10,OutC,Ret
  ]

memory :: InstructionList
memory =
  [Mark "memcopy"
  ,Mark "mem_copy"
  ,Mark "mem_move",Push 3,Swap,Store Nothing,Push 2,Swap,Store Nothing,Push 1,Swap,Store Nothing,Retrieve (Just 3),JumpNZ "memcopy_end"
  ,Retrieve (Just 1),Retrieve (Just 2),Sub Nothing,JumpZ "memcopy_end"
  ,Retrieve (Just 1),Retrieve (Just 2),Sub Nothing,JumpN "memcopy_loop_dest_greater_source_begin"
  ,Jump "memcopy_loop_source_greater_dest"
  ,Mark "memcopy_loop_source_greater_dest",Push 2,Retrieve Nothing,Push 1,Retrieve Nothing,Retrieve Nothing,Store Nothing,Push 2,Push 2,Retrieve Nothing,Push 1,Add Nothing,Store Nothing,Push 1,Push 1,Retrieve Nothing,Push 1,Add Nothing,Store Nothing,Push 3,Push 3,Retrieve Nothing,Push 1,Sub Nothing,Store Nothing,Retrieve (Just 3)
  ,JumpZ "memcopy_end"
  ,Jump "memcopy_loop_source_greater_dest"
  ,Mark "memcopy_loop_dest_greater_source_begin",Push 2,Push 2,Retrieve Nothing,Push 3,Retrieve Nothing,Add Nothing,Push 1,Sub Nothing,Store Nothing,Push 1,Push 1,Retrieve Nothing,Push 3,Retrieve Nothing,Add Nothing,Push 1,Sub Nothing,Store Nothing
  ,Mark "memcopy_loop_dest_greater_source",Push 2,Retrieve Nothing,Push 1,Retrieve Nothing,Retrieve Nothing,Store Nothing,Push 2,Push 2,Retrieve Nothing,Push 1,Sub Nothing,Store Nothing,Push 1,Push 1,Retrieve Nothing,Push 1,Sub Nothing,Store Nothing,Push 3,Push 3,Retrieve Nothing,Push 1,Sub Nothing,Store Nothing,Retrieve (Just 3)
  ,JumpZ "memcopy_end"
  ,Jump "memcopy_loop_dest_greater_source"
  ,Mark "memcopy_end",Ret
  ,Mark "mem_zero"
  ,Mark "mem_zero_start",Doub,JumpZ "mem_zero_end"
  ,Swap,Doub,Push 0,Store Nothing,Push 1,Add Nothing,Swap,Push 1,Sub Nothing,Jump "mem_zero_start"
  ,Mark "mem_zero_end",Pop,Pop,Ret
  ,Mark "numeriere",Push 2,Swap,Store Nothing,Push 1,Swap,Store Nothing,Push 1,Retrieve Nothing
  ,Mark "numeriere_start",Doub,Doub,Store Nothing,Push 1,Add Nothing,Doub,Push 2,Retrieve Nothing,Sub Nothing
  ,JumpZ "numeriere_end"
  ,Jump "numeriere_start"
  ,Mark "numeriere_end"
  ,Pop,Ret
  ]

prim :: InstructionList
prim =
  [Include "io"
  ,Mark "st",PushS "prim numbers [2-100]",Call "printsln",Push 0
  ,Mark "f_start_1",Test 100,JumpZ "f_finished_1",Doub,Add (Just 100),Push 1,Store Nothing,Add (Just 1),Jump "f_start_1"
  ,Mark "f_finished_1",Pop,Push 2,Mark "f_start_2",Test 11,JumpZ "f_finished_2",Push 2
  ,Mark "f_start_3",Test 100,JumpZ "f_finished_3",Doub,Push 1,Swap,Store Nothing,Swap,Doub,Push 2,Swap,Store Nothing,Swap,Retrieve (Just 1),Retrieve (Just 2),Mul Nothing,Add (Just 100),Push 0,Store Nothing,Add (Just 1),Jump "f_start_3"
  ,Mark "f_finished_3",Pop,Add (Just 1),Jump "f_start_2"
  ,Mark "f_finished_2",Pop,Push 2
  ,Mark "f_start_4",Test 100,JumpZ "f_finished_4",Doub,Add (Just 100),Retrieve Nothing,JumpZ "nodraw",Doub,OutN,PushS " ",Call "prints"
  ,Mark "nodraw",Add (Just 1),Jump "f_start_4",Mark "f_finished_4",Pop,Push 0,Call "printsln",Exit
  ]