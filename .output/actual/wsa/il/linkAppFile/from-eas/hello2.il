[ Push
    ( Literal 0 )
, Push
    ( Literal 10 )
, Push
    ( Literal 33 )
, Push
    ( Literal 100 )
, Push
    ( Literal 108 )
, Push
    ( Literal 114 )
, Push
    ( Literal 111 )
, Push
    ( Literal 119 )
, Push
    ( Literal 32 )
, Push
    ( Literal 44 )
, Push
    ( Literal 111 )
, Push
    ( Literal 108 )
, Push
    ( Literal 108 )
, Push
    ( Literal 101 )
, Push
    ( Literal 72 )
, Call "prints"
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
, Push
    ( Literal 10 )
, OutputChar
, Return
]