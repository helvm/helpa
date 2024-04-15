[ Mov "B"
    ( Literal 77 )
, Store "B"
    ( Literal 300 )
, Mov "C"
    ( Literal 300 )
, Load "A"
    ( Variable "C" )
, Load "A"
    ( Variable "C" )
, PutC
    ( Variable "A" )
, Mov "B"
    ( Literal 69 )
, Mov "C"
    ( Literal 30 )
, Store "B"
    ( Variable "C" )
, Store "B"
    ( Variable "C" )
, Load "A"
    ( Variable "C" )
, Load "A"
    ( Variable "C" )
, PutC
    ( Variable "A" )
, Add "A"
    ( Literal 8 )
, PutC
    ( Variable "A" )
, Load "A"
    ( Literal 556 )
, Add "A"
    ( Literal 10 )
, PutC
    ( Variable "A" )
, Exit
]