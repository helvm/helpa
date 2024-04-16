[ Include "io"
, Push 3
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
]