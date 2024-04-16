[ Mark "LOOP"
, Dup
, Push 0
, Push 1
, Sub Nothing
, Sub Nothing
, BranchNZ "WRITE"
, End
, Mark "WRITE"
, OutputChar
, Branch "LOOP"
]