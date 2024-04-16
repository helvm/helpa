[ Mark "memcopy"
, Mark "mem_copy"
, Mark "mem_move"
, Push 3
, Swap
, Store Nothing
, Push 2
, Swap
, Store Nothing
, Push 1
, Swap
, Store Nothing
, Load
    ( Just 3 )
, BranchNP "memcopy_end"
, Load
    ( Just 1 )
, Load
    ( Just 2 )
, Sub Nothing
, BranchZ "memcopy_end"
, Load
    ( Just 1 )
, Load
    ( Just 2 )
, Sub Nothing
, BranchM "memcopy_loop_dest_greater_source_begin"
, Branch "memcopy_loop_source_greater_dest"
, Mark "memcopy_loop_source_greater_dest"
, Push 2
, Load Nothing
, Push 1
, Load Nothing
, Load Nothing
, Store Nothing
, Push 2
, Push 2
, Load Nothing
, Push 1
, Add Nothing
, Store Nothing
, Push 1
, Push 1
, Load Nothing
, Push 1
, Add Nothing
, Store Nothing
, Push 3
, Push 3
, Load Nothing
, Push 1
, Sub Nothing
, Store Nothing
, Load
    ( Just 3 )
, BranchZ "memcopy_end"
, Branch "memcopy_loop_source_greater_dest"
, Mark "memcopy_loop_dest_greater_source_begin"
, Push 2
, Push 2
, Load Nothing
, Push 3
, Load Nothing
, Add Nothing
, Push 1
, Sub Nothing
, Store Nothing
, Push 1
, Push 1
, Load Nothing
, Push 3
, Load Nothing
, Add Nothing
, Push 1
, Sub Nothing
, Store Nothing
, Mark "memcopy_loop_dest_greater_source"
, Push 2
, Load Nothing
, Push 1
, Load Nothing
, Load Nothing
, Store Nothing
, Push 2
, Push 2
, Load Nothing
, Push 1
, Sub Nothing
, Store Nothing
, Push 1
, Push 1
, Load Nothing
, Push 1
, Sub Nothing
, Store Nothing
, Push 3
, Push 3
, Load Nothing
, Push 1
, Sub Nothing
, Store Nothing
, Load
    ( Just 3 )
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
, Push 0
, Store Nothing
, Push 1
, Add Nothing
, Swap
, Push 1
, Sub Nothing
, Branch "mem_zero_start"
, Mark "mem_zero_end"
, Pop
, Pop
, Return
, Mark "numeriere"
, Push 2
, Swap
, Store Nothing
, Push 1
, Swap
, Store Nothing
, Push 1
, Load Nothing
, Mark "numeriere_start"
, Dup
, Dup
, Store Nothing
, Push 1
, Add Nothing
, Dup
, Push 2
, Load Nothing
, Sub Nothing
, BranchZ "numeriere_end"
, Branch "numeriere_start"
, Mark "numeriere_end"
, Pop
, Return
]