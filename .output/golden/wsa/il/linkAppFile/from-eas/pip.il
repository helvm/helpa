[ Mark "LOOP"
, Dup
, Push
    ( Literal 0 )
, Push
    ( Literal 1 )
, Sub Nothing
, Sub Nothing
, BranchNZ "WRITE"
, End
, Mark "WRITE"
, OutputChar
, Branch "LOOP"
]