[ Mark "memcopy"
, Mark "mem_copy"
, Mark "mem_move"
, Push
    ( Literal 3 )
, Swap
, Store Nothing
, Push
    ( Literal 2 )
, Swap
, Store Nothing
, Push
    ( Literal 1 )
, Swap
, Store Nothing
, Load
    ( Just
        ( Literal 3 )
    )
, BranchNP "memcopy_end"
, Load
    ( Just
        ( Literal 1 )
    )
, Load
    ( Just
        ( Literal 2 )
    )
, Sub Nothing
, BranchZ "memcopy_end"
, Load
    ( Just
        ( Literal 1 )
    )
, Load
    ( Just
        ( Literal 2 )
    )
, Sub Nothing
, BranchM "memcopy_loop_dest_greater_source_begin"
, Branch "memcopy_loop_source_greater_dest"
, Mark "memcopy_loop_source_greater_dest"
, Push
    ( Literal 2 )
, Load Nothing
, Push
    ( Literal 1 )
, Load Nothing
, Load Nothing
, Store Nothing
, Push
    ( Literal 2 )
, Push
    ( Literal 2 )
, Load Nothing
, Push
    ( Literal 1 )
, Add Nothing
, Store Nothing
, Push
    ( Literal 1 )
, Push
    ( Literal 1 )
, Load Nothing
, Push
    ( Literal 1 )
, Add Nothing
, Store Nothing
, Push
    ( Literal 3 )
, Push
    ( Literal 3 )
, Load Nothing
, Push
    ( Literal 1 )
, Sub Nothing
, Store Nothing
, Load
    ( Just
        ( Literal 3 )
    )
, BranchZ "memcopy_end"
, Branch "memcopy_loop_source_greater_dest"
, Mark "memcopy_loop_dest_greater_source_begin"
, Push
    ( Literal 2 )
, Push
    ( Literal 2 )
, Load Nothing
, Push
    ( Literal 3 )
, Load Nothing
, Add Nothing
, Push
    ( Literal 1 )
, Sub Nothing
, Store Nothing
, Push
    ( Literal 1 )
, Push
    ( Literal 1 )
, Load Nothing
, Push
    ( Literal 3 )
, Load Nothing
, Add Nothing
, Push
    ( Literal 1 )
, Sub Nothing
, Store Nothing
, Mark "memcopy_loop_dest_greater_source"
, Push
    ( Literal 2 )
, Load Nothing
, Push
    ( Literal 1 )
, Load Nothing
, Load Nothing
, Store Nothing
, Push
    ( Literal 2 )
, Push
    ( Literal 2 )
, Load Nothing
, Push
    ( Literal 1 )
, Sub Nothing
, Store Nothing
, Push
    ( Literal 1 )
, Push
    ( Literal 1 )
, Load Nothing
, Push
    ( Literal 1 )
, Sub Nothing
, Store Nothing
, Push
    ( Literal 3 )
, Push
    ( Literal 3 )
, Load Nothing
, Push
    ( Literal 1 )
, Sub Nothing
, Store Nothing
, Load
    ( Just
        ( Literal 3 )
    )
, BranchZ "memcopy_end"
, Branch "memcopy_loop_dest_greater_source"
, Mark "memcopy_end"
, Return
, Mark "mem_zero"
, Mark "mem_zero_start"
, Dup
, BranchZ "mem_zero_end"
, Swap
, Dup
, Push
    ( Literal 0 )
, Store Nothing
, Push
    ( Literal 1 )
, Add Nothing
, Swap
, Push
    ( Literal 1 )
, Sub Nothing
, Branch "mem_zero_start"
, Mark "mem_zero_end"
, Pop
, Pop
, Return
, Mark "numeriere"
, Push
    ( Literal 2 )
, Swap
, Store Nothing
, Push
    ( Literal 1 )
, Swap
, Store Nothing
, Push
    ( Literal 1 )
, Load Nothing
, Mark "numeriere_start"
, Dup
, Dup
, Store Nothing
, Push
    ( Literal 1 )
, Add Nothing
, Dup
, Push
    ( Literal 2 )
, Load Nothing
, Sub Nothing
, BranchZ "numeriere_end"
, Branch "numeriere_start"
, Mark "numeriere_end"
, Pop
, Return
]