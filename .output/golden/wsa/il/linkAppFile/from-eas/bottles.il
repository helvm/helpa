[ Push
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