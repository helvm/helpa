[ Mark "st"
, PushS "prim numbers [2-100]"
, Call "printsln"
, Push 0
, Mark "f_start_1"
, Test 100
, BranchZ "f_finished_1"
, Dup
, Add
    ( Just 100 )
, Push 1
, Store Nothing
, Add
    ( Just 1 )
, Branch "f_start_1"
, Mark "f_finished_1"
, Pop
, Push 2
, Mark "f_start_2"
, Test 11
, BranchZ "f_finished_2"
, Push 2
, Mark "f_start_3"
, Test 100
, BranchZ "f_finished_3"
, Dup
, Push 1
, Swap
, Store Nothing
, Swap
, Dup
, Push 2
, Swap
, Store Nothing
, Swap
, Load
    ( Just 1 )
, Load
    ( Just 2 )
, Mul Nothing
, Add
    ( Just 100 )
, Push 0
, Store Nothing
, Add
    ( Just 1 )
, Branch "f_start_3"
, Mark "f_finished_3"
, Pop
, Add
    ( Just 1 )
, Branch "f_start_2"
, Mark "f_finished_2"
, Pop
, Push 2
, Mark "f_start_4"
, Test 100
, BranchZ "f_finished_4"
, Dup
, Add
    ( Just 100 )
, Load Nothing
, BranchZ "nodraw"
, Dup
, OutputNum
, PushS " "
, Call "prints"
, Mark "nodraw"
, Add
    ( Just 1 )
, Branch "f_start_4"
, Mark "f_finished_4"
, Pop
, Push 0
, Call "printsln"
, End
, Mark "prints"
, Dup
, BranchZ "prints_end"
, OutputChar
, Branch "prints"
, Mark "prints_end"
, Pop
, Return
, Mark "printsln"
, Call "prints"
, Push 10
, OutputChar
, Return
]