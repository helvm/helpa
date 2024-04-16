[ Push 3
, Mark "LOOP"
, Dup
, OutputNum
, PushS " bottles of beer on the wall, "
, Call "prints"
, Dup
, OutputNum
, PushS " bottles of beer"
, Call "printsln"
, PushS "Take one down, pass it around"
, Call "printsln"
, Push 1
, Sub Nothing
, Dup
, OutputNum
, PushS " bottles of beer on the wall"
, Call "printsln"
, PushS ""
, Call "printsln"
, Dup
, BranchNZ "LOOP"
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