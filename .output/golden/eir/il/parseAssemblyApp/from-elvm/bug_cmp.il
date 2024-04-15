[ Mov "A"
    ( Literal 300 )
, J CGT "ok" "A"
    ( Literal 255 )
, PutC
    ( Literal 120 )
, Exit
, Mark "ok"
, PutC
    ( Literal 111 )
, Exit
]