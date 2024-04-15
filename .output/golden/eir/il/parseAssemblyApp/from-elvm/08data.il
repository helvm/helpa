[ PText
, Mov "B"
    ( Variable "val" )
, Load "A"
    ( Variable "B" )
, PutC
    ( Variable "A" )
, Add "B"
    ( Literal 1 )
, Load "A"
    ( Variable "B" )
, PutC
    ( Variable "A" )
, Exit
, PData Nothing
, Mark "val"
, PString "hi"
]