[ Include "io"
, Mark "st"
, PushS
    ( Literal "prim numbers [2-100]" )
, Call "printsln"
, Push
    ( Literal 0 )
, Mark "f_start_1"
, Test 100
, BranchZ "f_finished_1"
, Dup
, Add
    ( Just
        ( Literal 100 )
    )
, Push
    ( Literal 1 )
, Store Nothing
, Add
    ( Just
        ( Literal 1 )
    )
, Branch "f_start_1"
, Mark "f_finished_1"
, Pop
, Push
    ( Literal 2 )
, Mark "f_start_2"
, Test 11
, BranchZ "f_finished_2"
, Push
    ( Literal 2 )
, Mark "f_start_3"
, Test 100
, BranchZ "f_finished_3"
, Dup
, Push
    ( Literal 1 )
, Swap
, Store Nothing
, Swap
, Dup
, Push
    ( Literal 2 )
, Swap
, Store Nothing
, Swap
, Load
    ( Just
        ( Literal 1 )
    )
, Load
    ( Just
        ( Literal 2 )
    )
, Mul Nothing
, Add
    ( Just
        ( Literal 100 )
    )
, Push
    ( Literal 0 )
, Store Nothing
, Add
    ( Just
        ( Literal 1 )
    )
, Branch "f_start_3"
, Mark "f_finished_3"
, Pop
, Add
    ( Just
        ( Literal 1 )
    )
, Branch "f_start_2"
, Mark "f_finished_2"
, Pop
, Push
    ( Literal 2 )
, Mark "f_start_4"
, Test 100
, BranchZ "f_finished_4"
, Dup
, Add
    ( Just
        ( Literal 100 )
    )
, Load Nothing
, BranchZ "nodraw"
, Dup
, OutputNum
, PushS
    ( Literal " " )
, Call "prints"
, Mark "nodraw"
, Add
    ( Just
        ( Literal 1 )
    )
, Branch "f_start_4"
, Mark "f_finished_4"
, Pop
, Push
    ( Literal 0 )
, Call "printsln"
, End
]