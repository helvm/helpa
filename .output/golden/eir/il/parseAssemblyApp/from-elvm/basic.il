[ Mov "A"
    ( Literal 33 )
, PutC
    ( Variable "A" )
, Mov "B"
    ( Variable "A" )
, PutC
    ( Variable "B" )
, Add "B"
    ( Variable "A" )
, Add "B"
    ( Literal
        ( -2 )
    )
, PutC
    ( Variable "B" )
, PutC
    ( Literal 88 )
, Add "A"
    ( Literal
        ( -23 )
    )
, Dump
, PutC
    ( Variable "A" )
, Exit
]