[ Include "io"
, Push
    ( Literal 3 )
, Mark "LOOP"
, Dup
, OutputNum
, PushS
    ( Literal " bottles of beer on the wall, " )
, Call "prints"
, Dup
, OutputNum
, PushS
    ( Literal " bottles of beer" )
, Call "printsln"
, PushS
    ( Literal "Take one down, pass it around" )
, Call "printsln"
, Push
    ( Literal 1 )
, Sub Nothing
, Dup
, OutputNum
, PushS
    ( Literal " bottles of beer on the wall" )
, Call "printsln"
, PushS
    ( Literal "" )
, Call "printsln"
, Dup
, BranchNZ "LOOP"
, End
]