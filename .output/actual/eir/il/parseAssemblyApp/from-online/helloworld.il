[ PText
, Mark "main"
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "BP"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
, Mov "BP"
    ( Variable "SP" )
, Sub "SP"
    ( Literal 1 )
, PFile 1 "-"
, PLoc 1 10 0
, PLoc 1 7 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "SP" )
, PData Nothing
, Mark ".L3"
, PString "Hello, world!\n"
, PText
, Mov "A"
    ( Variable ".L3" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Store "A"
    ( Variable "B" )
, PLoc 1 8 0
, Mark ".L0"
, PLoc 1 7 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "B" )
, J CEQ ".L4" "A"
    ( Literal 0 )
, Jmp ".L5"
, Mark ".L4"
, PLoc 1 8 0
, Jmp ".L2"
, Mark ".L5"
, PLoc 1 7 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "B" )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "A"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
, PutC
    ( Variable "A" )
, Add "SP"
    ( Literal 1 )
, PLoc 1 8 0
, Mark ".L1"
, PLoc 1 7 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Load "A"
    ( Variable "B" )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "A"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
, Add "A"
    ( Literal 1 )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Store "A"
    ( Variable "B" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, PLoc 1 8 0
, Jmp ".L0"
, Mark ".L2"
, PLoc 1 10 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "A" )
, Exit
, Exit
]