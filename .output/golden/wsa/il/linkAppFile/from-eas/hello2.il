[ Push 0
, Push 10
, Push 33
, Push 100
, Push 108
, Push 114
, Push 111
, Push 119
, Push 32
, Push 44
, Push 111
, Push 108
, Push 108
, Push 101
, Push 72
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
, Push 10
, OutputChar
, Return
]