[ Mark "loop"
, GetC "A"
, J CEQ "done" "A"
    ( Literal 0 )
, PutC
    ( Variable "A" )
, Jmp "loop"
, Mark "done"
, Exit
]