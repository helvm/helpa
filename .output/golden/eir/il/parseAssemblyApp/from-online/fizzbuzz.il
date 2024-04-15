[ PText
, Mark "my_div"
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
    ( Literal 52 )
, PFile 1 "-"
, PLoc 1 55 0
, PLoc 1 31 0
, PLoc 1 32 0
, PLoc 1 33 0
, PLoc 1 34 0
, Mov "A"
    ( Literal 1 )
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
, PLoc 1 32 0
, Mov "A"
    ( Variable "BP" )
, Add "A"
    ( Literal 16777168 )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, PLoc 1 34 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Store "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, PLoc 1 41 0
, PLoc 1 35 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777167 )
, Store "A"
    ( Variable "B" )
, PLoc 1 41 0
, Mark ".L0"
, PLoc 1 36 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
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
, PLoc 1 31 0
, Mov "A"
    ( Variable "BP" )
, Add "A"
    ( Literal 16777192 )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, PLoc 1 33 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777167 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Store "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, PLoc 1 37 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "SP" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
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
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777166 )
, Store "A"
    ( Variable "B" )
, PLoc 1 38 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
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
, PLoc 1 37 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777166 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CLT "A"
    ( Variable "B" )
, Mov "B"
    ( Literal 1 )
, J CNE ".L118" "A"
    ( Literal 0 )
, PLoc 1 38 0
, PLoc 1 37 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777166 )
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
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CLT "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, L CNE "B"
    ( Literal 0 )
, Mark ".L118"
, Mov "A"
    ( Variable "B" )
, J CEQ ".L119" "A"
    ( Literal 0 )
, PLoc 1 38 0
, Jmp ".L2"
, Mark ".L119"
, PLoc 1 40 0
, PLoc 1 32 0
, Mov "A"
    ( Variable "BP" )
, Add "A"
    ( Literal 16777168 )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, PLoc 1 33 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777167 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
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
, PLoc 1 40 0
, PLoc 1 32 0
, Mov "A"
    ( Variable "BP" )
, Add "A"
    ( Literal 16777168 )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, PLoc 1 33 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777167 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
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
, PLoc 1 40 0
, PLoc 1 32 0
, Mov "A"
    ( Variable "BP" )
, Add "A"
    ( Literal 16777168 )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, PLoc 1 40 0
, PLoc 1 33 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777167 )
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
, PLoc 1 40 0
, Mov "A"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Store "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, PLoc 1 41 0
, PLoc 1 37 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777166 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
, Store "A"
    ( Variable "B" )
, PLoc 1 41 0
, Mark ".L1"
, PLoc 1 35 0
, PLoc 1 33 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777167 )
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
    ( Literal 16777167 )
, Store "A"
    ( Variable "B" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, PLoc 1 41 0
, Jmp ".L0"
, Mark ".L2"
, PLoc 1 44 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "SP" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777165 )
, Mov "A"
    ( Literal 0 )
, Store "A"
    ( Variable "B" )
, PLoc 1 51 0
, Mark ".L3"
, PLoc 1 46 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "SP" )
, PLoc 1 31 0
, Mov "A"
    ( Variable "BP" )
, Add "A"
    ( Literal 16777192 )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, PLoc 1 33 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777167 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777164 )
, Store "A"
    ( Variable "B" )
, PLoc 1 49 0
, PLoc 1 47 0
, PLoc 1 46 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777164 )
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
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CLE "A"
    ( Variable "B" )
, J CEQ ".L120" "A"
    ( Literal 0 )
, PLoc 1 49 0
, PLoc 1 48 0
, PLoc 1 44 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777165 )
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
, PLoc 1 48 0
, PLoc 1 32 0
, Mov "A"
    ( Variable "BP" )
, Add "A"
    ( Literal 16777168 )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, PLoc 1 33 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777167 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777165 )
, Store "A"
    ( Variable "B" )
, PLoc 1 49 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
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
, PLoc 1 46 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777164 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Sub "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
, Store "A"
    ( Variable "B" )
, Mark ".L120"
, PLoc 1 51 0
, PLoc 1 33 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777167 )
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
, PLoc 1 51 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CEQ "A"
    ( Variable "B" )
, J CEQ ".L121" "A"
    ( Literal 0 )
, Jmp ".L5"
, Mark ".L121"
, Mark ".L4"
, PLoc 1 45 0
, PLoc 1 33 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777167 )
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
, Sub "A"
    ( Literal 1 )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777167 )
, Store "A"
    ( Variable "B" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, PLoc 1 51 0
, Jmp ".L3"
, Mark ".L5"
, PLoc 1 54 0
, PLoc 1 44 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777165 )
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
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 4 )
, Load "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Store "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, PLoc 1 55 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
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
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 4 )
, Load "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Add "A"
    ( Literal 1 )
, Store "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, PText
, Mark "__builtin_mul"
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
    ( Literal 53 )
, PLoc 1 90 0
, PLoc 1 59 0
, PLoc 1 63 0
, PLoc 1 60 0
, PLoc 1 55 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
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
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CLT "A"
    ( Variable "B" )
, J CEQ ".L122" "A"
    ( Literal 0 )
, PLoc 1 63 0
, PLoc 1 61 0
, PLoc 1 55 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777213 )
, Store "A"
    ( Variable "B" )
, PLoc 1 62 0
, PLoc 1 55 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
, Store "A"
    ( Variable "B" )
, PLoc 1 63 0
, PLoc 1 59 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777213 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
, Store "A"
    ( Variable "B" )
, Mark ".L122"
, PLoc 1 65 0
, PLoc 1 55 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
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
, PLoc 1 65 0
, Mov "A"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CEQ "A"
    ( Variable "B" )
, J CEQ ".L123" "A"
    ( Literal 0 )
, PLoc 1 55 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, Mark ".L123"
, PLoc 1 67 0
, PLoc 1 55 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
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
, PLoc 1 67 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CEQ "A"
    ( Variable "B" )
, J CEQ ".L124" "A"
    ( Literal 0 )
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "A" )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, Mark ".L124"
, PLoc 1 69 0
, PLoc 1 70 0
, PLoc 1 78 0
, PLoc 1 71 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Store "A"
    ( Variable "B" )
, Mov "A"
    ( Literal 1 )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
, Store "A"
    ( Variable "B" )
, PLoc 1 55 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777213 )
, Store "A"
    ( Variable "B" )
, PLoc 1 78 0
, Mark ".L6"
, PLoc 1 72 0
, PLoc 1 59 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777213 )
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
, PLoc 1 72 0
, PLoc 1 69 0
, Mov "A"
    ( Variable "BP" )
, Add "A"
    ( Literal 16777189 )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, PLoc 1 59 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Store "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, PLoc 1 73 0
, PLoc 1 59 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
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
, PLoc 1 73 0
, PLoc 1 70 0
, Mov "A"
    ( Variable "BP" )
, Add "A"
    ( Literal 16777165 )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, PLoc 1 59 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Store "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, PLoc 1 74 0
, PLoc 1 59 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777213 )
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
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777213 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777213 )
, Store "A"
    ( Variable "B" )
, PLoc 1 75 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "SP" )
, PLoc 1 59 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
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
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777164 )
, Store "A"
    ( Variable "B" )
, PLoc 1 76 0
, PLoc 1 75 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777164 )
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
, PLoc 1 59 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CLT "A"
    ( Variable "B" )
, Mov "B"
    ( Literal 1 )
, J CNE ".L125" "A"
    ( Literal 0 )
, PLoc 1 76 0
, PLoc 1 55 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
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
, PLoc 1 75 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777164 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CLT "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, L CNE "B"
    ( Literal 0 )
, Mark ".L125"
, Mov "A"
    ( Variable "B" )
, J CEQ ".L126" "A"
    ( Literal 0 )
, PLoc 1 76 0
, Jmp ".L8"
, Mark ".L126"
, PLoc 1 78 0
, PLoc 1 75 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777164 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
, Store "A"
    ( Variable "B" )
, PLoc 1 78 0
, Mark ".L7"
, PLoc 1 71 0
, PLoc 1 59 0
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
, PLoc 1 78 0
, Jmp ".L6"
, Mark ".L8"
, PLoc 1 81 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "SP" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777163 )
, Mov "A"
    ( Literal 0 )
, Store "A"
    ( Variable "B" )
, PLoc 1 87 0
, Mark ".L9"
, PLoc 1 85 0
, PLoc 1 83 0
, PLoc 1 70 0
, Mov "A"
    ( Variable "BP" )
, Add "A"
    ( Literal 16777165 )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, PLoc 1 59 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
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
, PLoc 1 55 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CLE "A"
    ( Variable "B" )
, J CEQ ".L127" "A"
    ( Literal 0 )
, PLoc 1 85 0
, PLoc 1 84 0
, PLoc 1 81 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777163 )
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
, PLoc 1 84 0
, PLoc 1 69 0
, Mov "A"
    ( Variable "BP" )
, Add "A"
    ( Literal 16777189 )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, PLoc 1 59 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777163 )
, Store "A"
    ( Variable "B" )
, PLoc 1 85 0
, PLoc 1 55 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
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
, PLoc 1 85 0
, PLoc 1 70 0
, Mov "A"
    ( Variable "BP" )
, Add "A"
    ( Literal 16777165 )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, PLoc 1 59 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Sub "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
, Store "A"
    ( Variable "B" )
, Mark ".L127"
, PLoc 1 87 0
, PLoc 1 59 0
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
, PLoc 1 87 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CEQ "A"
    ( Variable "B" )
, J CEQ ".L128" "A"
    ( Literal 0 )
, Jmp ".L11"
, Mark ".L128"
, Mark ".L10"
, PLoc 1 82 0
, PLoc 1 59 0
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
, Sub "A"
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
, PLoc 1 87 0
, Jmp ".L9"
, Mark ".L11"
, PLoc 1 90 0
, PLoc 1 81 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777163 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, PText
, Mark "__builtin_div"
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
    ( Literal 2 )
, PLoc 1 98 0
, PLoc 1 94 0
, PLoc 1 90 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
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
, PLoc 1 94 0
, Mov "A"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CEQ "A"
    ( Variable "B" )
, J CEQ ".L129" "A"
    ( Literal 0 )
, PLoc 1 90 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, Mark ".L129"
, PLoc 1 96 0
, PLoc 1 97 0
, Mov "A"
    ( Variable "BP" )
, Add "A"
    ( Literal 16777214 )
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
, PLoc 1 90 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
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
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
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
, Mov "A"
    ( Variable ".L130" )
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
, Jmp "my_div"
, Mark ".L130"
, Mov "A"
    ( Variable "B" )
, Add "SP"
    ( Literal 3 )
, PLoc 1 98 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, PText
, Mark "__builtin_mod"
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
    ( Literal 2 )
, PLoc 1 104 0
, PLoc 1 102 0
, PLoc 1 103 0
, Mov "A"
    ( Variable "BP" )
, Add "A"
    ( Literal 16777214 )
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
, PLoc 1 98 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
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
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
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
, Mov "A"
    ( Variable ".L131" )
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
, Jmp "my_div"
, Mark ".L131"
, Mov "A"
    ( Variable "B" )
, Add "SP"
    ( Literal 3 )
, PLoc 1 104 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, PData
    ( Just 0 )
, Mark "__builtin_bits_table"
, PLong 8388608
, PLong 4194304
, PLong 2097152
, PLong 1048576
, PLong 524288
, PLong 262144
, PLong 131072
, PLong 65536
, PLong 32768
, PLong 16384
, PLong 8192
, PLong 4096
, PLong 2048
, PLong 1024
, PLong 512
, PLong 256
, PLong 128
, PLong 64
, PLong 32
, PLong 16
, PLong 8
, PLong 4
, PLong 2
, PLong 1
, PText
, Mark "__builtin_and"
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
    ( Literal 5 )
, PLoc 1 127 0
, PLoc 1 119 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "SP" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Mov "A"
    ( Literal 0 )
, Store "A"
    ( Variable "B" )
, PLoc 1 124 0
, PLoc 1 120 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "SP" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
, Mov "A"
    ( Literal 0 )
, Store "A"
    ( Variable "B" )
, PLoc 1 124 0
, Mark ".L12"
, PLoc 1 120 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
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
, Mov "A"
    ( Literal 24 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CLT "A"
    ( Variable "B" )
, J CEQ ".L132" "A"
    ( Literal 0 )
, Jmp ".L133"
, Mark ".L132"
, PLoc 1 124 0
, Jmp ".L14"
, Mark ".L133"
, PLoc 1 121 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "SP" )
, PLoc 1 104 0
, Mov "A"
    ( Variable "__builtin_bits_table" )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, PLoc 1 120 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777213 )
, Store "A"
    ( Variable "B" )
, PLoc 1 122 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "SP" )
, PLoc 1 121 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777213 )
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
, PLoc 1 104 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CLE "A"
    ( Variable "B" )
, J CEQ ".L134" "A"
    ( Literal 0 )
, PLoc 1 122 0
, PLoc 1 104 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
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
, PLoc 1 122 0
, PLoc 1 121 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777213 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Sub "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
, Store "A"
    ( Variable "B" )
, PLoc 1 122 0
, Mov "A"
    ( Literal 1 )
, Jmp ".L135"
, Mark ".L134"
, Mov "A"
    ( Literal 0 )
, Mark ".L135"
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777212 )
, Store "A"
    ( Variable "B" )
, PLoc 1 123 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "SP" )
, PLoc 1 121 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777213 )
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
, PLoc 1 104 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CLE "A"
    ( Variable "B" )
, J CEQ ".L136" "A"
    ( Literal 0 )
, PLoc 1 123 0
, PLoc 1 104 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
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
, PLoc 1 123 0
, PLoc 1 121 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777213 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Sub "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
, Store "A"
    ( Variable "B" )
, PLoc 1 123 0
, Mov "A"
    ( Literal 1 )
, Jmp ".L137"
, Mark ".L136"
, Mov "A"
    ( Literal 0 )
, Mark ".L137"
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777211 )
, Store "A"
    ( Variable "B" )
, PLoc 1 124 0
, PLoc 1 122 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777212 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Literal 0 )
, J CEQ ".L138" "A"
    ( Literal 0 )
, PLoc 1 123 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777211 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, L CNE "B"
    ( Literal 0 )
, Mark ".L138"
, Mov "A"
    ( Variable "B" )
, J CEQ ".L139" "A"
    ( Literal 0 )
, PLoc 1 124 0
, PLoc 1 119 0
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
, PLoc 1 121 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777213 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Store "A"
    ( Variable "B" )
, Mark ".L139"
, PLoc 1 124 0
, Mark ".L13"
, PLoc 1 120 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
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
    ( Literal 16777214 )
, Store "A"
    ( Variable "B" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, PLoc 1 124 0
, Jmp ".L12"
, Mark ".L14"
, PLoc 1 127 0
, PLoc 1 119 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, PText
, Mark "__builtin_or"
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
    ( Literal 5 )
, PLoc 1 139 0
, PLoc 1 131 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "SP" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Mov "A"
    ( Literal 0 )
, Store "A"
    ( Variable "B" )
, PLoc 1 136 0
, PLoc 1 132 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "SP" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
, Mov "A"
    ( Literal 0 )
, Store "A"
    ( Variable "B" )
, PLoc 1 136 0
, Mark ".L15"
, PLoc 1 132 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
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
, Mov "A"
    ( Literal 24 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CLT "A"
    ( Variable "B" )
, J CEQ ".L140" "A"
    ( Literal 0 )
, Jmp ".L141"
, Mark ".L140"
, PLoc 1 136 0
, Jmp ".L17"
, Mark ".L141"
, PLoc 1 133 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "SP" )
, PLoc 1 104 0
, Mov "A"
    ( Variable "__builtin_bits_table" )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, PLoc 1 132 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777213 )
, Store "A"
    ( Variable "B" )
, PLoc 1 134 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "SP" )
, PLoc 1 133 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777213 )
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
, PLoc 1 127 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CLE "A"
    ( Variable "B" )
, J CEQ ".L142" "A"
    ( Literal 0 )
, PLoc 1 134 0
, PLoc 1 127 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
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
, PLoc 1 134 0
, PLoc 1 133 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777213 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Sub "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
, Store "A"
    ( Variable "B" )
, PLoc 1 134 0
, Mov "A"
    ( Literal 1 )
, Jmp ".L143"
, Mark ".L142"
, Mov "A"
    ( Literal 0 )
, Mark ".L143"
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777212 )
, Store "A"
    ( Variable "B" )
, PLoc 1 135 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "SP" )
, PLoc 1 133 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777213 )
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
, PLoc 1 127 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CLE "A"
    ( Variable "B" )
, J CEQ ".L144" "A"
    ( Literal 0 )
, PLoc 1 135 0
, PLoc 1 127 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
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
, PLoc 1 135 0
, PLoc 1 133 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777213 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Sub "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
, Store "A"
    ( Variable "B" )
, PLoc 1 135 0
, Mov "A"
    ( Literal 1 )
, Jmp ".L145"
, Mark ".L144"
, Mov "A"
    ( Literal 0 )
, Mark ".L145"
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777211 )
, Store "A"
    ( Variable "B" )
, PLoc 1 136 0
, PLoc 1 134 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777212 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Literal 1 )
, J CNE ".L146" "A"
    ( Literal 0 )
, PLoc 1 135 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777211 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, L CNE "B"
    ( Literal 0 )
, Mark ".L146"
, Mov "A"
    ( Variable "B" )
, J CEQ ".L147" "A"
    ( Literal 0 )
, PLoc 1 136 0
, PLoc 1 131 0
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
, PLoc 1 133 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777213 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Store "A"
    ( Variable "B" )
, Mark ".L147"
, PLoc 1 136 0
, Mark ".L16"
, PLoc 1 132 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
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
    ( Literal 16777214 )
, Store "A"
    ( Variable "B" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, PLoc 1 136 0
, Jmp ".L15"
, Mark ".L17"
, PLoc 1 139 0
, PLoc 1 131 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, PText
, Mark "__builtin_xor"
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
    ( Literal 5 )
, PLoc 1 151 0
, PLoc 1 143 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "SP" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Mov "A"
    ( Literal 0 )
, Store "A"
    ( Variable "B" )
, PLoc 1 148 0
, PLoc 1 144 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "SP" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
, Mov "A"
    ( Literal 0 )
, Store "A"
    ( Variable "B" )
, PLoc 1 148 0
, Mark ".L18"
, PLoc 1 144 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
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
, Mov "A"
    ( Literal 24 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CLT "A"
    ( Variable "B" )
, J CEQ ".L148" "A"
    ( Literal 0 )
, Jmp ".L149"
, Mark ".L148"
, PLoc 1 148 0
, Jmp ".L20"
, Mark ".L149"
, PLoc 1 145 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "SP" )
, PLoc 1 104 0
, Mov "A"
    ( Variable "__builtin_bits_table" )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, PLoc 1 144 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777213 )
, Store "A"
    ( Variable "B" )
, PLoc 1 146 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "SP" )
, PLoc 1 145 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777213 )
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
, PLoc 1 139 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CLE "A"
    ( Variable "B" )
, J CEQ ".L150" "A"
    ( Literal 0 )
, PLoc 1 146 0
, PLoc 1 139 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
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
, PLoc 1 146 0
, PLoc 1 145 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777213 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Sub "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
, Store "A"
    ( Variable "B" )
, PLoc 1 146 0
, Mov "A"
    ( Literal 1 )
, Jmp ".L151"
, Mark ".L150"
, Mov "A"
    ( Literal 0 )
, Mark ".L151"
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777212 )
, Store "A"
    ( Variable "B" )
, PLoc 1 147 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "SP" )
, PLoc 1 145 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777213 )
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
, PLoc 1 139 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CLE "A"
    ( Variable "B" )
, J CEQ ".L152" "A"
    ( Literal 0 )
, PLoc 1 147 0
, PLoc 1 139 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
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
, PLoc 1 147 0
, PLoc 1 145 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777213 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Sub "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
, Store "A"
    ( Variable "B" )
, PLoc 1 147 0
, Mov "A"
    ( Literal 1 )
, Jmp ".L153"
, Mark ".L152"
, Mov "A"
    ( Literal 0 )
, Mark ".L153"
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777211 )
, Store "A"
    ( Variable "B" )
, PLoc 1 148 0
, PLoc 1 146 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777212 )
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
, PLoc 1 147 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777211 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CNE "A"
    ( Variable "B" )
, J CEQ ".L154" "A"
    ( Literal 0 )
, PLoc 1 148 0
, PLoc 1 143 0
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
, PLoc 1 145 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777213 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Store "A"
    ( Variable "B" )
, Mark ".L154"
, PLoc 1 148 0
, Mark ".L19"
, PLoc 1 144 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
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
    ( Literal 16777214 )
, Store "A"
    ( Variable "B" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, PLoc 1 148 0
, Jmp ".L18"
, Mark ".L20"
, PLoc 1 151 0
, PLoc 1 143 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, PText
, Mark "__builtin_not"
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
    ( Literal 4 )
, PLoc 1 162 0
, PLoc 1 155 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "SP" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Mov "A"
    ( Literal 0 )
, Store "A"
    ( Variable "B" )
, PLoc 1 159 0
, PLoc 1 156 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "SP" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
, Mov "A"
    ( Literal 0 )
, Store "A"
    ( Variable "B" )
, PLoc 1 159 0
, Mark ".L21"
, PLoc 1 156 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
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
, Mov "A"
    ( Literal 24 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CLT "A"
    ( Variable "B" )
, J CEQ ".L155" "A"
    ( Literal 0 )
, Jmp ".L156"
, Mark ".L155"
, PLoc 1 159 0
, Jmp ".L23"
, Mark ".L156"
, PLoc 1 157 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "SP" )
, PLoc 1 104 0
, Mov "A"
    ( Variable "__builtin_bits_table" )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, PLoc 1 156 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777213 )
, Store "A"
    ( Variable "B" )
, PLoc 1 158 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "SP" )
, PLoc 1 157 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777213 )
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
, PLoc 1 151 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CLE "A"
    ( Variable "B" )
, J CEQ ".L157" "A"
    ( Literal 0 )
, PLoc 1 158 0
, PLoc 1 151 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
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
, PLoc 1 158 0
, PLoc 1 157 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777213 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Sub "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
, Store "A"
    ( Variable "B" )
, PLoc 1 158 0
, Mov "A"
    ( Literal 1 )
, Jmp ".L158"
, Mark ".L157"
, Mov "A"
    ( Literal 0 )
, Mark ".L158"
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777212 )
, Store "A"
    ( Variable "B" )
, PLoc 1 159 0
, PLoc 1 158 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777212 )
, Load "A"
    ( Variable "B" )
, L CEQ "A"
    ( Literal 0 )
, J CEQ ".L159" "A"
    ( Literal 0 )
, PLoc 1 159 0
, PLoc 1 155 0
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
, PLoc 1 157 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777213 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Store "A"
    ( Variable "B" )
, Mark ".L159"
, PLoc 1 159 0
, Mark ".L22"
, PLoc 1 156 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
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
    ( Literal 16777214 )
, Store "A"
    ( Variable "B" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, PLoc 1 159 0
, Jmp ".L21"
, Mark ".L23"
, PLoc 1 162 0
, PLoc 1 155 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, PText
, Mark "__builtin_shl"
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
    ( Literal 4 )
, PLoc 1 173 0
, PLoc 1 166 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "SP" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Mov "A"
    ( Literal 0 )
, Store "A"
    ( Variable "B" )
, PLoc 1 170 0
, PLoc 1 167 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "SP" )
, PLoc 1 162 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
, Store "A"
    ( Variable "B" )
, PLoc 1 170 0
, Mark ".L24"
, PLoc 1 167 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
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
, Mov "A"
    ( Literal 24 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CLT "A"
    ( Variable "B" )
, J CEQ ".L160" "A"
    ( Literal 0 )
, Jmp ".L161"
, Mark ".L160"
, PLoc 1 170 0
, Jmp ".L26"
, Mark ".L161"
, PLoc 1 168 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "SP" )
, PLoc 1 104 0
, Mov "A"
    ( Variable "__builtin_bits_table" )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, PLoc 1 167 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777213 )
, Store "A"
    ( Variable "B" )
, PLoc 1 169 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "SP" )
, PLoc 1 168 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777213 )
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
, PLoc 1 162 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CLE "A"
    ( Variable "B" )
, J CEQ ".L162" "A"
    ( Literal 0 )
, PLoc 1 169 0
, PLoc 1 162 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
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
, PLoc 1 169 0
, PLoc 1 168 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777213 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Sub "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
, Store "A"
    ( Variable "B" )
, PLoc 1 169 0
, Mov "A"
    ( Literal 1 )
, Jmp ".L163"
, Mark ".L162"
, Mov "A"
    ( Literal 0 )
, Mark ".L163"
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777212 )
, Store "A"
    ( Variable "B" )
, PLoc 1 170 0
, PLoc 1 169 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777212 )
, Load "A"
    ( Variable "B" )
, J CEQ ".L164" "A"
    ( Literal 0 )
, PLoc 1 170 0
, PLoc 1 166 0
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
, PLoc 1 170 0
, PLoc 1 104 0
, Mov "A"
    ( Variable "__builtin_bits_table" )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, PLoc 1 170 0
, PLoc 1 167 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
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
, PLoc 1 162 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Sub "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Store "A"
    ( Variable "B" )
, Mark ".L164"
, PLoc 1 170 0
, Mark ".L25"
, PLoc 1 167 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
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
    ( Literal 16777214 )
, Store "A"
    ( Variable "B" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, PLoc 1 170 0
, Jmp ".L24"
, Mark ".L26"
, PLoc 1 173 0
, PLoc 1 166 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, PText
, Mark "__builtin_shr"
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
    ( Literal 4 )
, PLoc 1 184 0
, PLoc 1 177 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "SP" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Mov "A"
    ( Literal 0 )
, Store "A"
    ( Variable "B" )
, PLoc 1 181 0
, PLoc 1 178 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "SP" )
, PLoc 1 173 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
, Store "A"
    ( Variable "B" )
, PLoc 1 181 0
, Mark ".L27"
, PLoc 1 178 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
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
, Mov "A"
    ( Literal 24 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CLT "A"
    ( Variable "B" )
, J CEQ ".L165" "A"
    ( Literal 0 )
, Jmp ".L166"
, Mark ".L165"
, PLoc 1 181 0
, Jmp ".L29"
, Mark ".L166"
, PLoc 1 179 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "SP" )
, PLoc 1 104 0
, Mov "A"
    ( Variable "__builtin_bits_table" )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, PLoc 1 179 0
, PLoc 1 178 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
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
, PLoc 1 173 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Sub "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777213 )
, Store "A"
    ( Variable "B" )
, PLoc 1 180 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "SP" )
, PLoc 1 179 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777213 )
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
, PLoc 1 173 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CLE "A"
    ( Variable "B" )
, J CEQ ".L167" "A"
    ( Literal 0 )
, PLoc 1 180 0
, PLoc 1 173 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
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
, PLoc 1 180 0
, PLoc 1 179 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777213 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Sub "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
, Store "A"
    ( Variable "B" )
, PLoc 1 180 0
, Mov "A"
    ( Literal 1 )
, Jmp ".L168"
, Mark ".L167"
, Mov "A"
    ( Literal 0 )
, Mark ".L168"
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777212 )
, Store "A"
    ( Variable "B" )
, PLoc 1 181 0
, PLoc 1 180 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777212 )
, Load "A"
    ( Variable "B" )
, J CEQ ".L169" "A"
    ( Literal 0 )
, PLoc 1 181 0
, PLoc 1 177 0
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
, PLoc 1 181 0
, PLoc 1 104 0
, Mov "A"
    ( Variable "__builtin_bits_table" )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, PLoc 1 178 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Store "A"
    ( Variable "B" )
, Mark ".L169"
, PLoc 1 181 0
, Mark ".L28"
, PLoc 1 178 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
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
    ( Literal 16777214 )
, Store "A"
    ( Variable "B" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, PLoc 1 181 0
, Jmp ".L27"
, Mark ".L29"
, PLoc 1 184 0
, PLoc 1 177 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, PText
, Mark "print_str"
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
, PLoc 1 218 0
, Mark ".L30"
, PLoc 1 184 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "B" )
, J CEQ ".L170" "A"
    ( Literal 0 )
, Jmp ".L171"
, Mark ".L170"
, PLoc 1 218 0
, Jmp ".L32"
, Mark ".L171"
, PLoc 1 184 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
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
, PLoc 1 218 0
, Mark ".L31"
, PLoc 1 184 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
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
    ( Literal 2 )
, Store "A"
    ( Variable "B" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, PLoc 1 218 0
, Jmp ".L30"
, Mark ".L32"
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, PText
, Mark "stringify_int"
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
, PLoc 1 229 0
, PLoc 1 223 0
, Mov "A"
    ( Literal 0 )
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
, PLoc 1 218 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
, Load "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Store "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, PLoc 1 227 0
, Mark ".L33"
, PLoc 1 225 0
, PLoc 1 218 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
, Load "A"
    ( Variable "B" )
, Sub "A"
    ( Literal 1 )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
, Store "A"
    ( Variable "B" )
, PLoc 1 226 0
, PLoc 1 218 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
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
, PLoc 1 226 0
, Mov "A"
    ( Literal 10 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, Mov "A"
    ( Variable ".L172" )
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
, Jmp "__builtin_mod"
, Mark ".L172"
, Mov "A"
    ( Variable "B" )
, Add "SP"
    ( Literal 2 )
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
, Mov "A"
    ( Literal 48 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
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
, PLoc 1 218 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
, Load "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Store "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, PLoc 1 227 0
, PLoc 1 218 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
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
, PLoc 1 227 0
, Mov "A"
    ( Literal 10 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, Mov "A"
    ( Variable ".L173" )
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
, Jmp "__builtin_div"
, Mark ".L173"
, Mov "A"
    ( Variable "B" )
, Add "SP"
    ( Literal 2 )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
, Store "A"
    ( Variable "B" )
, PLoc 1 218 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
, Load "A"
    ( Variable "B" )
, J CEQ ".L174" "A"
    ( Literal 0 )
, PLoc 1 227 0
, Jmp ".L33"
, Mark ".L174"
, Mark ".L34"
, PLoc 1 229 0
, PLoc 1 218 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, PText
, Mark "print_int"
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
    ( Literal 32 )
, PLoc 1 234 0
, PLoc 1 233 0
, PLoc 1 234 0
, PLoc 1 233 0
, Mov "A"
    ( Variable "BP" )
, Add "A"
    ( Literal 16777184 )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, PLoc 1 234 0
, Mov "A"
    ( Literal 32 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, Mov "A"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Sub "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
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
, PLoc 1 229 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
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
, Mov "A"
    ( Variable ".L175" )
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
, Jmp "stringify_int"
, Mark ".L175"
, Mov "A"
    ( Variable "B" )
, Add "SP"
    ( Literal 2 )
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
, Mov "A"
    ( Variable ".L176" )
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
, Jmp "print_str"
, Mark ".L176"
, Mov "A"
    ( Variable "B" )
, Add "SP"
    ( Literal 1 )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, PText
, Mark "stringify_hex"
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
    ( Literal 2 )
, PLoc 1 253 0
, PLoc 1 238 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "SP" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Mov "A"
    ( Literal 0 )
, Store "A"
    ( Variable "B" )
, PLoc 1 239 0
, PLoc 1 240 0
, Mov "A"
    ( Literal 0 )
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
, PLoc 1 234 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
, Load "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Store "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, PLoc 1 243 0
, PLoc 1 241 0
, PLoc 1 234 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
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
, PLoc 1 241 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CLT "A"
    ( Variable "B" )
, J CEQ ".L177" "A"
    ( Literal 0 )
, PLoc 1 243 0
, PLoc 1 242 0
, Mov "A"
    ( Literal 0 )
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
, PLoc 1 234 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Sub "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
, Store "A"
    ( Variable "B" )
, PLoc 1 243 0
, Mov "A"
    ( Literal 1 )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Store "A"
    ( Variable "B" )
, Mark ".L177"
, PLoc 1 249 0
, Mark ".L35"
, PLoc 1 246 0
, PLoc 1 234 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
, Load "A"
    ( Variable "B" )
, Sub "A"
    ( Literal 1 )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
, Store "A"
    ( Variable "B" )
, PLoc 1 247 0
, PLoc 1 234 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
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
, PLoc 1 247 0
, Mov "A"
    ( Literal 16 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, Mov "A"
    ( Variable ".L178" )
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
, Jmp "__builtin_mod"
, Mark ".L178"
, Mov "A"
    ( Variable "B" )
, Add "SP"
    ( Literal 2 )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
, Store "A"
    ( Variable "B" )
, PLoc 1 248 0
, PLoc 1 239 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
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
, PLoc 1 248 0
, Mov "A"
    ( Literal 10 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CLT "A"
    ( Variable "B" )
, J CEQ ".L179" "A"
    ( Literal 0 )
, PLoc 1 239 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
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
, PLoc 1 248 0
, Mov "A"
    ( Literal 48 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Jmp ".L180"
, Mark ".L179"
, PLoc 1 239 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
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
, PLoc 1 248 0
, Mov "A"
    ( Literal 10 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Sub "A"
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
, Mov "A"
    ( Literal 97 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mark ".L180"
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
, PLoc 1 234 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
, Load "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Store "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, PLoc 1 249 0
, PLoc 1 234 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
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
, PLoc 1 249 0
, Mov "A"
    ( Literal 16 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, Mov "A"
    ( Variable ".L181" )
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
, Jmp "__builtin_div"
, Mark ".L181"
, Mov "A"
    ( Variable "B" )
, Add "SP"
    ( Literal 2 )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
, Store "A"
    ( Variable "B" )
, PLoc 1 234 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
, Load "A"
    ( Variable "B" )
, J CEQ ".L182" "A"
    ( Literal 0 )
, PLoc 1 249 0
, Jmp ".L35"
, Mark ".L182"
, Mark ".L36"
, PLoc 1 251 0
, PLoc 1 238 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Load "A"
    ( Variable "B" )
, J CEQ ".L183" "A"
    ( Literal 0 )
, PLoc 1 251 0
, Mov "A"
    ( Literal 45 )
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
, PLoc 1 234 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
, Load "A"
    ( Variable "B" )
, Sub "A"
    ( Literal 1 )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
, Store "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Store "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mark ".L183"
, PLoc 1 253 0
, PLoc 1 234 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, PText
, Mark "isspace"
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
, PLoc 1 265 0
, PLoc 1 253 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
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
, PLoc 1 265 0
, Mov "A"
    ( Literal 12 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CEQ "A"
    ( Variable "B" )
, Mov "B"
    ( Literal 1 )
, J CNE ".L188" "A"
    ( Literal 0 )
, PLoc 1 253 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
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
, PLoc 1 265 0
, Mov "A"
    ( Literal 10 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CEQ "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, L CNE "B"
    ( Literal 0 )
, Mark ".L188"
, Mov "A"
    ( Variable "B" )
, Mov "B"
    ( Literal 1 )
, J CNE ".L187" "A"
    ( Literal 0 )
, PLoc 1 253 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
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
, PLoc 1 265 0
, Mov "A"
    ( Literal 13 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CEQ "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, L CNE "B"
    ( Literal 0 )
, Mark ".L187"
, Mov "A"
    ( Variable "B" )
, Mov "B"
    ( Literal 1 )
, J CNE ".L186" "A"
    ( Literal 0 )
, PLoc 1 253 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
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
, PLoc 1 265 0
, Mov "A"
    ( Literal 9 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CEQ "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, L CNE "B"
    ( Literal 0 )
, Mark ".L186"
, Mov "A"
    ( Variable "B" )
, Mov "B"
    ( Literal 1 )
, J CNE ".L185" "A"
    ( Literal 0 )
, PLoc 1 253 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
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
, PLoc 1 265 0
, Mov "A"
    ( Literal 11 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CEQ "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, L CNE "B"
    ( Literal 0 )
, Mark ".L185"
, Mov "A"
    ( Variable "B" )
, Mov "B"
    ( Literal 1 )
, J CNE ".L184" "A"
    ( Literal 0 )
, PLoc 1 253 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
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
, PLoc 1 265 0
, Mov "A"
    ( Literal 32 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CEQ "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, L CNE "B"
    ( Literal 0 )
, Mark ".L184"
, Mov "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, PText
, Mark "isdigit"
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
, PLoc 1 270 0
, Mov "A"
    ( Literal 48 )
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
, PLoc 1 265 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CLE "A"
    ( Variable "B" )
, Mov "B"
    ( Literal 0 )
, J CEQ ".L189" "A"
    ( Literal 0 )
, PLoc 1 270 0
, PLoc 1 265 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
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
, PLoc 1 270 0
, Mov "A"
    ( Literal 57 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CLE "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, L CNE "B"
    ( Literal 0 )
, Mark ".L189"
, Mov "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, PText
, Mark "isxdigit"
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
, PLoc 1 274 0
, PLoc 1 270 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
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
, Mov "A"
    ( Variable ".L192" )
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
, Jmp "isdigit"
, Mark ".L192"
, Mov "A"
    ( Variable "B" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Literal 1 )
, J CNE ".L191" "A"
    ( Literal 0 )
, PLoc 1 274 0
, Mov "A"
    ( Literal 97 )
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
, PLoc 1 270 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CLE "A"
    ( Variable "B" )
, Mov "B"
    ( Literal 0 )
, J CEQ ".L193" "A"
    ( Literal 0 )
, PLoc 1 274 0
, PLoc 1 270 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
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
, PLoc 1 274 0
, Mov "A"
    ( Literal 102 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CLE "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, L CNE "B"
    ( Literal 0 )
, Mark ".L193"
, Mov "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, L CNE "B"
    ( Literal 0 )
, Mark ".L191"
, Mov "A"
    ( Variable "B" )
, Mov "B"
    ( Literal 1 )
, J CNE ".L190" "A"
    ( Literal 0 )
, Mov "A"
    ( Literal 65 )
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
, PLoc 1 270 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CLE "A"
    ( Variable "B" )
, Mov "B"
    ( Literal 0 )
, J CEQ ".L194" "A"
    ( Literal 0 )
, PLoc 1 274 0
, PLoc 1 270 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
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
, PLoc 1 274 0
, Mov "A"
    ( Literal 70 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CLE "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, L CNE "B"
    ( Literal 0 )
, Mark ".L194"
, Mov "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, L CNE "B"
    ( Literal 0 )
, Mark ".L190"
, Mov "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, PText
, Mark "isupper"
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
, PLoc 1 278 0
, Mov "A"
    ( Literal 65 )
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
, PLoc 1 274 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CLE "A"
    ( Variable "B" )
, Mov "B"
    ( Literal 0 )
, J CEQ ".L195" "A"
    ( Literal 0 )
, PLoc 1 278 0
, PLoc 1 274 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
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
, PLoc 1 278 0
, Mov "A"
    ( Literal 90 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CLE "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, L CNE "B"
    ( Literal 0 )
, Mark ".L195"
, Mov "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, PText
, Mark "isalpha"
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
, PLoc 1 282 0
, Mov "A"
    ( Literal 97 )
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
, PLoc 1 278 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CLE "A"
    ( Variable "B" )
, Mov "B"
    ( Literal 0 )
, J CEQ ".L197" "A"
    ( Literal 0 )
, PLoc 1 282 0
, PLoc 1 278 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
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
, PLoc 1 282 0
, Mov "A"
    ( Literal 122 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CLE "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, L CNE "B"
    ( Literal 0 )
, Mark ".L197"
, Mov "A"
    ( Variable "B" )
, Mov "B"
    ( Literal 1 )
, J CNE ".L196" "A"
    ( Literal 0 )
, Mov "A"
    ( Literal 65 )
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
, PLoc 1 278 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CLE "A"
    ( Variable "B" )
, Mov "B"
    ( Literal 0 )
, J CEQ ".L198" "A"
    ( Literal 0 )
, PLoc 1 282 0
, PLoc 1 278 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
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
, PLoc 1 282 0
, Mov "A"
    ( Literal 90 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CLE "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, L CNE "B"
    ( Literal 0 )
, Mark ".L198"
, Mov "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, L CNE "B"
    ( Literal 0 )
, Mark ".L196"
, Mov "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, PText
, Mark "isalnum"
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
, PLoc 1 286 0
, PLoc 1 282 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
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
, Mov "A"
    ( Variable ".L200" )
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
, Jmp "isalpha"
, Mark ".L200"
, Mov "A"
    ( Variable "B" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Literal 1 )
, J CNE ".L199" "A"
    ( Literal 0 )
, PLoc 1 286 0
, PLoc 1 282 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
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
, Mov "A"
    ( Variable ".L201" )
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
, Jmp "isdigit"
, Mark ".L201"
, Mov "A"
    ( Variable "B" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, L CNE "B"
    ( Literal 0 )
, Mark ".L199"
, Mov "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, PText
, Mark "isprint"
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
, PLoc 1 290 0
, PLoc 1 286 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
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
, Mov "A"
    ( Variable ".L203" )
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
, Jmp "isspace"
, Mark ".L203"
, Mov "A"
    ( Variable "B" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Literal 1 )
, J CNE ".L202" "A"
    ( Literal 0 )
, PLoc 1 290 0
, Mov "A"
    ( Literal 32 )
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
, PLoc 1 286 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CLE "A"
    ( Variable "B" )
, Mov "B"
    ( Literal 0 )
, J CEQ ".L204" "A"
    ( Literal 0 )
, PLoc 1 290 0
, PLoc 1 286 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
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
, PLoc 1 290 0
, Mov "A"
    ( Literal 127 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CLT "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, L CNE "B"
    ( Literal 0 )
, Mark ".L204"
, Mov "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, L CNE "B"
    ( Literal 0 )
, Mark ".L202"
, Mov "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, PText
, Mark "abort"
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
, PLoc 1 316 0
, Mov "A"
    ( Literal 1 )
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
, Exit
, Add "SP"
    ( Literal 1 )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, PText
, Mark "malloc"
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
, PLoc 1 326 0
, PLoc 1 320 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "SP" )
, PLoc 1 290 0
, Mov "B"
    ( Variable "_edata" )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Store "A"
    ( Variable "B" )
, PLoc 1 321 0
, PLoc 1 290 0
, Mov "B"
    ( Variable "_edata" )
, Load "A"
    ( Variable "B" )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, PLoc 1 316 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Mov "B"
    ( Variable "_edata" )
, Store "A"
    ( Variable "B" )
, PLoc 1 324 0
, PLoc 1 322 0
, PLoc 1 290 0
, Mov "B"
    ( Variable "_edata" )
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
, PLoc 1 320 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CLT "A"
    ( Variable "B" )
, J CEQ ".L205" "A"
    ( Literal 0 )
, PLoc 1 324 0
, PLoc 1 323 0
, PData Nothing
, Mark ".L206"
, PString "no memory!\n"
, PText
, Mov "A"
    ( Variable ".L206" )
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
, Mov "A"
    ( Variable ".L207" )
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
, Jmp "print_str"
, Mark ".L207"
, Mov "A"
    ( Variable "B" )
, Add "SP"
    ( Literal 1 )
, PLoc 1 324 0
, Mov "A"
    ( Literal 1 )
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
, Exit
, Add "SP"
    ( Literal 1 )
, Mark ".L205"
, PLoc 1 326 0
, PLoc 1 320 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, PText
, Mark "calloc"
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
, PLoc 1 330 0
, PLoc 1 326 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
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
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, Mov "A"
    ( Variable ".L208" )
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
, Jmp "__builtin_mul"
, Mark ".L208"
, Mov "A"
    ( Variable "B" )
, Add "SP"
    ( Literal 2 )
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
, Mov "A"
    ( Variable ".L209" )
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
, Jmp "malloc"
, Mark ".L209"
, Mov "A"
    ( Variable "B" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, PText
, Mark "free"
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
, PLoc 1 330 0
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, PText
, Mark "strtol"
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
    ( Literal 7 )
, PLoc 1 411 0
, PLoc 1 340 0
, PLoc 1 341 0
, PLoc 1 342 0
, PLoc 1 343 0
, PLoc 1 352 0
, PLoc 1 349 0
, PLoc 1 330 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 4 )
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
, PLoc 1 349 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CLT "A"
    ( Variable "B" )
, Mov "B"
    ( Literal 1 )
, J CNE ".L211" "A"
    ( Literal 0 )
, PLoc 1 330 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 4 )
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
, PLoc 1 349 0
, Mov "A"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CEQ "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, L CNE "B"
    ( Literal 0 )
, Mark ".L211"
, Mov "A"
    ( Variable "B" )
, Mov "B"
    ( Literal 1 )
, J CNE ".L210" "A"
    ( Literal 0 )
, Mov "A"
    ( Literal 36 )
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
, PLoc 1 330 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 4 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CLT "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, L CNE "B"
    ( Literal 0 )
, Mark ".L210"
, Mov "A"
    ( Variable "B" )
, J CEQ ".L212" "A"
    ( Literal 0 )
, PLoc 1 352 0
, PLoc 1 350 0
, PLoc 1 330 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
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
, PLoc 1 350 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CNE "A"
    ( Variable "B" )
, J CEQ ".L213" "A"
    ( Literal 0 )
, PLoc 1 330 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
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
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
, Load "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Store "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mark ".L213"
, PLoc 1 352 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "A" )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, Mark ".L212"
, PLoc 1 360 0
, PLoc 1 330 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Store "A"
    ( Variable "B" )
, PLoc 1 362 0
, Mark ".L37"
, PLoc 1 340 0
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
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777212 )
, Store "A"
    ( Variable "B" )
, PLoc 1 362 0
, PLoc 1 342 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777212 )
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
, Mov "A"
    ( Variable ".L214" )
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
, Jmp "isspace"
, Mark ".L214"
, Mov "A"
    ( Variable "B" )
, Add "SP"
    ( Literal 1 )
, J CEQ ".L215" "A"
    ( Literal 0 )
, PLoc 1 362 0
, Jmp ".L37"
, Mark ".L215"
, Mark ".L38"
, PLoc 1 369 0
, PLoc 1 364 0
, PLoc 1 342 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777212 )
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
, PLoc 1 364 0
, Mov "A"
    ( Literal 45 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CEQ "A"
    ( Variable "B" )
, J CEQ ".L216" "A"
    ( Literal 0 )
, PLoc 1 366 0
, PLoc 1 365 0
, Mov "A"
    ( Literal 1 )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777211 )
, Store "A"
    ( Variable "B" )
, PLoc 1 366 0
, PLoc 1 340 0
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
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777212 )
, Store "A"
    ( Variable "B" )
, Jmp ".L217"
, Mark ".L216"
, PLoc 1 369 0
, PLoc 1 368 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777211 )
, Store "A"
    ( Variable "B" )
, PLoc 1 369 0
, PLoc 1 342 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777212 )
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
, PLoc 1 369 0
, Mov "A"
    ( Literal 43 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CEQ "A"
    ( Variable "B" )
, J CEQ ".L218" "A"
    ( Literal 0 )
, PLoc 1 340 0
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
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777212 )
, Store "A"
    ( Variable "B" )
, Mark ".L218"
, Mark ".L217"
, PLoc 1 376 0
, PLoc 1 372 0
, PLoc 1 330 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 4 )
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
, PLoc 1 372 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CEQ "A"
    ( Variable "B" )
, Mov "B"
    ( Literal 1 )
, J CNE ".L221" "A"
    ( Literal 0 )
, PLoc 1 330 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 4 )
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
, PLoc 1 372 0
, Mov "A"
    ( Literal 16 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CEQ "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, L CNE "B"
    ( Literal 0 )
, Mark ".L221"
, Mov "A"
    ( Variable "B" )
, Mov "B"
    ( Literal 0 )
, J CEQ ".L220" "A"
    ( Literal 0 )
, PLoc 1 342 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777212 )
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
, PLoc 1 372 0
, Mov "A"
    ( Literal 48 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CEQ "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, L CNE "B"
    ( Literal 0 )
, Mark ".L220"
, Mov "A"
    ( Variable "B" )
, Mov "B"
    ( Literal 0 )
, J CEQ ".L219" "A"
    ( Literal 0 )
, PLoc 1 340 0
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
, PLoc 1 372 0
, Mov "A"
    ( Literal 120 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CEQ "A"
    ( Variable "B" )
, Mov "B"
    ( Literal 1 )
, J CNE ".L222" "A"
    ( Literal 0 )
, PLoc 1 340 0
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
, PLoc 1 372 0
, Mov "A"
    ( Literal 88 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CEQ "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, L CNE "B"
    ( Literal 0 )
, Mark ".L222"
, Mov "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, L CNE "B"
    ( Literal 0 )
, Mark ".L219"
, Mov "A"
    ( Variable "B" )
, J CEQ ".L223" "A"
    ( Literal 0 )
, PLoc 1 376 0
, PLoc 1 374 0
, PLoc 1 340 0
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
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, PLoc 1 374 0
, Mov "A"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777212 )
, Store "A"
    ( Variable "B" )
, PLoc 1 375 0
, PLoc 1 340 0
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
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, PLoc 1 375 0
, Mov "A"
    ( Literal 2 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Store "A"
    ( Variable "B" )
, PLoc 1 376 0
, Mov "A"
    ( Literal 16 )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 4 )
, Store "A"
    ( Variable "B" )
, Mark ".L223"
, PLoc 1 378 0
, PLoc 1 330 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 4 )
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
, PLoc 1 378 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CEQ "A"
    ( Variable "B" )
, J CEQ ".L224" "A"
    ( Literal 0 )
, PLoc 1 342 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777212 )
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
, PLoc 1 378 0
, Mov "A"
    ( Literal 48 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CEQ "A"
    ( Variable "B" )
, J CEQ ".L225" "A"
    ( Literal 0 )
, Mov "A"
    ( Literal 8 )
, Jmp ".L226"
, Mark ".L225"
, Mov "A"
    ( Literal 10 )
, Mark ".L226"
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 4 )
, Store "A"
    ( Variable "B" )
, Mark ".L224"
, PLoc 1 386 0
, PLoc 1 343 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777211 )
, Load "A"
    ( Variable "B" )
, J CEQ ".L227" "A"
    ( Literal 0 )
, PLoc 1 386 0
, PLoc 1 384 0
, PLoc 1 382 0
, Mov "A"
    ( Literal 0 )
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
, PLoc 1 343 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777209 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CLT "A"
    ( Variable "B" )
, J CEQ ".L228" "A"
    ( Literal 0 )
, PLoc 1 384 0
, PLoc 1 383 0
, PLoc 1 343 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777209 )
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
, PLoc 1 330 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 4 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Sub "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777209 )
, Store "A"
    ( Variable "B" )
, PLoc 1 384 0
, PLoc 1 341 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777213 )
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
, PLoc 1 384 0
, Mov "A"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777213 )
, Store "A"
    ( Variable "B" )
, Mark ".L228"
, PLoc 1 386 0
, Mov "A"
    ( Literal 0 )
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
, PLoc 1 343 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777209 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Sub "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777209 )
, Store "A"
    ( Variable "B" )
, Mark ".L227"
, PLoc 1 406 0
, PLoc 1 388 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
, Store "A"
    ( Variable "B" )
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777210 )
, Store "A"
    ( Variable "B" )
, PLoc 1 406 0
, Mark ".L39"
, PLoc 1 389 0
, PLoc 1 342 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777212 )
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
, Mov "A"
    ( Variable ".L229" )
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
, Jmp "isdigit"
, Mark ".L229"
, Mov "A"
    ( Variable "B" )
, Add "SP"
    ( Literal 1 )
, J CEQ ".L230" "A"
    ( Literal 0 )
, PLoc 1 389 0
, PLoc 1 342 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777212 )
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
, PLoc 1 389 0
, Mov "A"
    ( Literal 48 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Sub "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777212 )
, Store "A"
    ( Variable "B" )
, Jmp ".L231"
, Mark ".L230"
, PLoc 1 342 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777212 )
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
, Mov "A"
    ( Variable ".L232" )
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
, Jmp "isalpha"
, Mark ".L232"
, Mov "A"
    ( Variable "B" )
, Add "SP"
    ( Literal 1 )
, J CEQ ".L233" "A"
    ( Literal 0 )
, PLoc 1 389 0
, PLoc 1 342 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777212 )
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
, PLoc 1 389 0
, PLoc 1 342 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777212 )
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
, Mov "A"
    ( Variable ".L234" )
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
, Jmp "isupper"
, Mark ".L234"
, Mov "A"
    ( Variable "B" )
, Add "SP"
    ( Literal 1 )
, J CEQ ".L235" "A"
    ( Literal 0 )
, PLoc 1 389 0
, Mov "A"
    ( Literal 65 )
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
, Mov "A"
    ( Literal 10 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Sub "A"
    ( Variable "B" )
, Jmp ".L236"
, Mark ".L235"
, Mov "A"
    ( Literal 97 )
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
, Mov "A"
    ( Literal 10 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Sub "A"
    ( Variable "B" )
, Mark ".L236"
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Sub "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777212 )
, Store "A"
    ( Variable "B" )
, Jmp ".L237"
, Mark ".L233"
, Jmp ".L41"
, Mark ".L237"
, Mark ".L231"
, PLoc 1 395 0
, PLoc 1 330 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 4 )
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
, PLoc 1 342 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777212 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CLE "A"
    ( Variable "B" )
, J CEQ ".L238" "A"
    ( Literal 0 )
, PLoc 1 395 0
, Jmp ".L41"
, Mark ".L238"
, PLoc 1 397 0
, PLoc 1 343 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777210 )
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
, PLoc 1 397 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CLT "A"
    ( Variable "B" )
, J CEQ ".L239" "A"
    ( Literal 0 )
, Jmp ".L40"
, Mark ".L239"
, PLoc 1 406 0
, PLoc 1 343 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777211 )
, Load "A"
    ( Variable "B" )
, J CEQ ".L240" "A"
    ( Literal 0 )
, PLoc 1 402 0
, PLoc 1 400 0
, Mov "A"
    ( Literal 1 )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777210 )
, Store "A"
    ( Variable "B" )
, PLoc 1 401 0
, PLoc 1 341 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
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
, PLoc 1 401 0
, PLoc 1 330 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 4 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, Mov "A"
    ( Variable ".L241" )
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
, Jmp "__builtin_mul"
, Mark ".L241"
, Mov "A"
    ( Variable "B" )
, Add "SP"
    ( Literal 2 )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
, Store "A"
    ( Variable "B" )
, PLoc 1 402 0
, PLoc 1 341 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
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
, PLoc 1 402 0
, PLoc 1 342 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777212 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Sub "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
, Store "A"
    ( Variable "B" )
, Jmp ".L242"
, Mark ".L240"
, PLoc 1 406 0
, PLoc 1 404 0
, Mov "A"
    ( Literal 1 )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777210 )
, Store "A"
    ( Variable "B" )
, PLoc 1 405 0
, PLoc 1 341 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
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
, PLoc 1 405 0
, PLoc 1 330 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 4 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, Mov "A"
    ( Variable ".L243" )
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
, Jmp "__builtin_mul"
, Mark ".L243"
, Mov "A"
    ( Variable "B" )
, Add "SP"
    ( Literal 2 )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
, Store "A"
    ( Variable "B" )
, PLoc 1 406 0
, PLoc 1 341 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
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
, PLoc 1 406 0
, PLoc 1 342 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777212 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
, Store "A"
    ( Variable "B" )
, Mark ".L242"
, PLoc 1 406 0
, Mark ".L40"
, PLoc 1 388 0
, PLoc 1 340 0
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
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777212 )
, Store "A"
    ( Variable "B" )
, PLoc 1 406 0
, Jmp ".L39"
, Mark ".L41"
, PLoc 1 409 0
, PLoc 1 330 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
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
, PLoc 1 409 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CNE "A"
    ( Variable "B" )
, J CEQ ".L244" "A"
    ( Literal 0 )
, PLoc 1 343 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777210 )
, Load "A"
    ( Variable "B" )
, J CEQ ".L245" "A"
    ( Literal 0 )
, PLoc 1 409 0
, PLoc 1 340 0
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
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, PLoc 1 409 0
, Mov "A"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Sub "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Jmp ".L246"
, Mark ".L245"
, PLoc 1 330 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
, Load "A"
    ( Variable "B" )
, Mark ".L246"
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
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
, Load "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Store "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mark ".L244"
, PLoc 1 411 0
, PLoc 1 341 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, PText
, Mark "strtoul"
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
, PLoc 1 415 0
, PLoc 1 411 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 4 )
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
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
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
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
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
, Mov "A"
    ( Variable ".L247" )
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
, Jmp "strtol"
, Mark ".L247"
, Mov "A"
    ( Variable "B" )
, Add "SP"
    ( Literal 3 )
, Mov "B"
    ( Variable "A" )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, PText
, Mark "strtoll"
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
, PLoc 1 419 0
, PLoc 1 415 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 4 )
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
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
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
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
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
, Mov "A"
    ( Variable ".L248" )
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
, Jmp "strtol"
, Mark ".L248"
, Mov "A"
    ( Variable "B" )
, Add "SP"
    ( Literal 3 )
, Mov "B"
    ( Variable "A" )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, PText
, Mark "strtoull"
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
, PLoc 1 423 0
, PLoc 1 419 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 4 )
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
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
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
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
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
, Mov "A"
    ( Variable ".L249" )
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
, Jmp "strtol"
, Mark ".L249"
, Mov "A"
    ( Variable "B" )
, Add "SP"
    ( Literal 3 )
, Mov "B"
    ( Variable "A" )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, PText
, Mark "atoi"
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
    ( Literal 2 )
, PLoc 1 433 0
, PLoc 1 427 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "SP" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Mov "A"
    ( Literal 0 )
, Store "A"
    ( Variable "B" )
, PLoc 1 428 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "SP" )
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
, Store "A"
    ( Variable "B" )
, PLoc 1 431 0
, Mark ".L42"
, PLoc 1 429 0
, Mov "A"
    ( Literal 48 )
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
, PLoc 1 423 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
, Load "A"
    ( Variable "B" )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, PLoc 1 428 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CLE "A"
    ( Variable "B" )
, Mov "B"
    ( Literal 0 )
, J CEQ ".L250" "A"
    ( Literal 0 )
, PLoc 1 429 0
, PLoc 1 423 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
, Load "A"
    ( Variable "B" )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, PLoc 1 428 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
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
, PLoc 1 429 0
, Mov "A"
    ( Literal 57 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CLE "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, L CNE "B"
    ( Literal 0 )
, Mark ".L250"
, Mov "A"
    ( Variable "B" )
, J CEQ ".L251" "A"
    ( Literal 0 )
, PLoc 1 431 0
, PLoc 1 430 0
, PLoc 1 427 0
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
, PLoc 1 430 0
, Mov "A"
    ( Literal 3 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, Mov "A"
    ( Variable ".L252" )
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
, Jmp "__builtin_shl"
, Mark ".L252"
, Mov "A"
    ( Variable "B" )
, Add "SP"
    ( Literal 2 )
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
, PLoc 1 427 0
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
, PLoc 1 430 0
, Mov "A"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, Mov "A"
    ( Variable ".L253" )
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
, Jmp "__builtin_shl"
, Mark ".L253"
, Mov "A"
    ( Variable "B" )
, Add "SP"
    ( Literal 2 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Store "A"
    ( Variable "B" )
, PLoc 1 431 0
, PLoc 1 427 0
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
, PLoc 1 431 0
, PLoc 1 423 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
, Load "A"
    ( Variable "B" )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, PLoc 1 431 0
, PLoc 1 428 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
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
    ( Literal 16777214 )
, Store "A"
    ( Variable "B" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
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
, PLoc 1 431 0
, Mov "A"
    ( Literal 48 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Sub "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Store "A"
    ( Variable "B" )
, Jmp ".L254"
, Mark ".L251"
, Jmp ".L43"
, Mark ".L254"
, Jmp ".L42"
, Mark ".L43"
, PLoc 1 433 0
, PLoc 1 427 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, PText
, Mark "getenv"
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
, PLoc 1 437 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "A" )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, PText
, Mark "qsort"
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
    ( Literal 6 )
, PLoc 1 479 0
, PLoc 1 442 0
, PLoc 1 437 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
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
, PLoc 1 442 0
, Mov "A"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CLE "A"
    ( Variable "B" )
, J CEQ ".L255" "A"
    ( Literal 0 )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, Mark ".L255"
, PLoc 1 444 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "SP" )
, PLoc 1 437 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Store "A"
    ( Variable "B" )
, PLoc 1 445 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "SP" )
, PLoc 1 444 0
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
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, PLoc 1 445 0
, PLoc 1 437 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
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
, PLoc 1 445 0
, Mov "A"
    ( Literal 2 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, Mov "A"
    ( Variable ".L256" )
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
, Jmp "__builtin_div"
, Mark ".L256"
, Mov "A"
    ( Variable "B" )
, Add "SP"
    ( Literal 2 )
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
, PLoc 1 437 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 4 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, Mov "A"
    ( Variable ".L257" )
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
, Jmp "__builtin_mul"
, Mark ".L257"
, Mov "A"
    ( Variable "B" )
, Add "SP"
    ( Literal 2 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
, Store "A"
    ( Variable "B" )
, PLoc 1 446 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "SP" )
, PLoc 1 444 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777213 )
, Store "A"
    ( Variable "B" )
, PLoc 1 447 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "SP" )
, PLoc 1 444 0
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
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, PLoc 1 447 0
, PLoc 1 437 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
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
, PLoc 1 447 0
, Mov "A"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Sub "A"
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
, PLoc 1 437 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 4 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, Mov "A"
    ( Variable ".L258" )
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
, Jmp "__builtin_mul"
, Mark ".L258"
, Mov "A"
    ( Variable "B" )
, Add "SP"
    ( Literal 2 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777212 )
, Store "A"
    ( Variable "B" )
, PLoc 1 474 0
, Mark ".L44"
, PLoc 1 457 0
, Mark ".L47"
, Mov "A"
    ( Literal 32768 )
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
, PLoc 1 445 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
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
, PLoc 1 446 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777213 )
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
, PLoc 1 437 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 5 )
, Load "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Mov "A"
    ( Variable ".L259" )
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
, Jmp "C"
, Mark ".L259"
, Mov "A"
    ( Variable "B" )
, Add "SP"
    ( Literal 2 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CLE "A"
    ( Variable "B" )
, J CEQ ".L260" "A"
    ( Literal 0 )
, PLoc 1 457 0
, PLoc 1 446 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777213 )
, Load "A"
    ( Variable "B" )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, PLoc 1 437 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 4 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777213 )
, Store "A"
    ( Variable "B" )
, Jmp ".L261"
, Mark ".L260"
, PLoc 1 457 0
, Jmp ".L48"
, Mark ".L261"
, Jmp ".L47"
, Mark ".L48"
, PLoc 1 459 0
, Mark ".L49"
, PLoc 1 445 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
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
, PLoc 1 447 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777212 )
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
, PLoc 1 437 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 5 )
, Load "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Mov "A"
    ( Variable ".L262" )
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
, Jmp "C"
, Mark ".L262"
, Mov "A"
    ( Variable "B" )
, Add "SP"
    ( Literal 2 )
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
, PLoc 1 459 0
, Mov "A"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Sub "A"
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
, Mov "A"
    ( Literal 32768 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CLT "A"
    ( Variable "B" )
, J CEQ ".L263" "A"
    ( Literal 0 )
, PLoc 1 447 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777212 )
, Load "A"
    ( Variable "B" )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, PLoc 1 437 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 4 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Sub "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777212 )
, Store "A"
    ( Variable "B" )
, Jmp ".L264"
, Mark ".L263"
, PLoc 1 459 0
, Jmp ".L50"
, Mark ".L264"
, Jmp ".L49"
, Mark ".L50"
, PLoc 1 462 0
, PLoc 1 447 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777212 )
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
, PLoc 1 446 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777213 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CLE "A"
    ( Variable "B" )
, J CEQ ".L265" "A"
    ( Literal 0 )
, PLoc 1 462 0
, Jmp ".L46"
, Mark ".L265"
, PLoc 1 467 0
, PLoc 1 464 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "SP" )
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777211 )
, Store "A"
    ( Variable "B" )
, PLoc 1 467 0
, Mark ".L51"
, PLoc 1 464 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777211 )
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
, PLoc 1 437 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 4 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CLT "A"
    ( Variable "B" )
, J CEQ ".L266" "A"
    ( Literal 0 )
, Jmp ".L267"
, Mark ".L266"
, PLoc 1 467 0
, Jmp ".L53"
, Mark ".L267"
, PLoc 1 465 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "SP" )
, PLoc 1 446 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777213 )
, Load "A"
    ( Variable "B" )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, PLoc 1 464 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777211 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777210 )
, Store "A"
    ( Variable "B" )
, PLoc 1 466 0
, PLoc 1 447 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777212 )
, Load "A"
    ( Variable "B" )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, PLoc 1 464 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777211 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
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
, PLoc 1 466 0
, PLoc 1 446 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777213 )
, Load "A"
    ( Variable "B" )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, PLoc 1 464 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777211 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Store "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, PLoc 1 467 0
, PLoc 1 465 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777210 )
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
, PLoc 1 467 0
, PLoc 1 447 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777212 )
, Load "A"
    ( Variable "B" )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, PLoc 1 464 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777211 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Store "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, PLoc 1 467 0
, Mark ".L52"
, PLoc 1 464 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777211 )
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
    ( Literal 16777211 )
, Store "A"
    ( Variable "B" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, PLoc 1 467 0
, Jmp ".L51"
, Mark ".L53"
, PLoc 1 469 0
, PLoc 1 445 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
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
, PLoc 1 446 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777213 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CEQ "A"
    ( Variable "B" )
, J CEQ ".L268" "A"
    ( Literal 0 )
, PLoc 1 469 0
, PLoc 1 447 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777212 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
, Store "A"
    ( Variable "B" )
, Jmp ".L269"
, Mark ".L268"
, PLoc 1 469 0
, PLoc 1 445 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
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
, PLoc 1 447 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777212 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CEQ "A"
    ( Variable "B" )
, J CEQ ".L270" "A"
    ( Literal 0 )
, PLoc 1 469 0
, PLoc 1 446 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777213 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
, Store "A"
    ( Variable "B" )
, Mark ".L270"
, Mark ".L269"
, PLoc 1 473 0
, PLoc 1 446 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777213 )
, Load "A"
    ( Variable "B" )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, PLoc 1 437 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 4 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777213 )
, Store "A"
    ( Variable "B" )
, PLoc 1 474 0
, PLoc 1 447 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777212 )
, Load "A"
    ( Variable "B" )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, PLoc 1 437 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 4 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Sub "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777212 )
, Store "A"
    ( Variable "B" )
, PLoc 1 474 0
, Mark ".L45"
, Jmp ".L44"
, Mark ".L46"
, PLoc 1 477 0
, PLoc 1 447 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777212 )
, Load "A"
    ( Variable "B" )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, PLoc 1 437 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 4 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777212 )
, Store "A"
    ( Variable "B" )
, PLoc 1 478 0
, PLoc 1 437 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 5 )
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
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 4 )
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
, PLoc 1 478 0
, PLoc 1 446 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777213 )
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
, PLoc 1 444 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Sub "A"
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
, PLoc 1 437 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 4 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, Mov "A"
    ( Variable ".L271" )
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
, Jmp "__builtin_div"
, Mark ".L271"
, Mov "A"
    ( Variable "B" )
, Add "SP"
    ( Literal 2 )
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
, PLoc 1 444 0
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
, Mov "A"
    ( Variable ".L272" )
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
, Jmp "qsort"
, Mark ".L272"
, Mov "A"
    ( Variable "B" )
, Add "SP"
    ( Literal 4 )
, PLoc 1 479 0
, PLoc 1 437 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 5 )
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
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 4 )
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
, PLoc 1 479 0
, PLoc 1 444 0
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
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, PLoc 1 479 0
, PLoc 1 437 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
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
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 4 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, Mov "A"
    ( Variable ".L273" )
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
, Jmp "__builtin_mul"
, Mark ".L273"
, Mov "A"
    ( Variable "B" )
, Add "SP"
    ( Literal 2 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
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
, PLoc 1 447 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777212 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Sub "A"
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
, PLoc 1 437 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 4 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, Mov "A"
    ( Variable ".L274" )
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
, Jmp "__builtin_div"
, Mark ".L274"
, Mov "A"
    ( Variable "B" )
, Add "SP"
    ( Literal 2 )
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
, PLoc 1 447 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777212 )
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
, Mov "A"
    ( Variable ".L275" )
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
, Jmp "qsort"
, Mark ".L275"
, Mov "A"
    ( Variable "B" )
, Add "SP"
    ( Literal 4 )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, PText
, Mark "memset"
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
, PLoc 1 491 0
, PLoc 1 487 0
, PLoc 1 489 0
, PLoc 1 488 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Store "A"
    ( Variable "B" )
, PLoc 1 489 0
, Mark ".L54"
, PLoc 1 488 0
, PLoc 1 487 0
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
, PLoc 1 479 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 4 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CLT "A"
    ( Variable "B" )
, J CEQ ".L276" "A"
    ( Literal 0 )
, Jmp ".L277"
, Mark ".L276"
, PLoc 1 489 0
, Jmp ".L56"
, Mark ".L277"
, PLoc 1 479 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
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
, PLoc 1 489 0
, PLoc 1 479 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
, Load "A"
    ( Variable "B" )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, PLoc 1 487 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Store "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, PLoc 1 489 0
, Mark ".L55"
, PLoc 1 488 0
, PLoc 1 487 0
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
, PLoc 1 489 0
, Jmp ".L54"
, Mark ".L56"
, PLoc 1 491 0
, PLoc 1 479 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, PText
, Mark "memcpy"
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
, PLoc 1 499 0
, PLoc 1 495 0
, PLoc 1 497 0
, PLoc 1 496 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Store "A"
    ( Variable "B" )
, PLoc 1 497 0
, Mark ".L57"
, PLoc 1 496 0
, PLoc 1 495 0
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
, PLoc 1 491 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 4 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CLT "A"
    ( Variable "B" )
, J CEQ ".L278" "A"
    ( Literal 0 )
, Jmp ".L279"
, Mark ".L278"
, PLoc 1 497 0
, Jmp ".L59"
, Mark ".L279"
, PLoc 1 491 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
, Load "A"
    ( Variable "B" )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, PLoc 1 495 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
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
, PLoc 1 497 0
, PLoc 1 491 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
, Load "A"
    ( Variable "B" )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, PLoc 1 495 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Store "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, PLoc 1 497 0
, Mark ".L58"
, PLoc 1 496 0
, PLoc 1 495 0
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
, PLoc 1 497 0
, Jmp ".L57"
, Mark ".L59"
, PLoc 1 499 0
, PLoc 1 491 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, PText
, Mark "strlen"
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
, PLoc 1 505 0
, PLoc 1 503 0
, PLoc 1 504 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Store "A"
    ( Variable "B" )
, Mark ".L60"
, PLoc 1 499 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
, Load "A"
    ( Variable "B" )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, PLoc 1 503 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "B" )
, J CEQ ".L280" "A"
    ( Literal 0 )
, Jmp ".L281"
, Mark ".L280"
, PLoc 1 504 0
, Jmp ".L62"
, Mark ".L281"
, Mark ".L61"
, PLoc 1 503 0
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
, PLoc 1 504 0
, Jmp ".L60"
, Mark ".L62"
, PLoc 1 505 0
, PLoc 1 503 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, PText
, Mark "strcat"
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
, PLoc 1 513 0
, PLoc 1 509 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "SP" )
, PLoc 1 505 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Store "A"
    ( Variable "B" )
, PLoc 1 510 0
, Mark ".L63"
, PLoc 1 505 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "B" )
, J CEQ ".L282" "A"
    ( Literal 0 )
, Jmp ".L283"
, Mark ".L282"
, PLoc 1 510 0
, Jmp ".L65"
, Mark ".L283"
, Mark ".L64"
, PLoc 1 505 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
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
    ( Literal 2 )
, Store "A"
    ( Variable "B" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, PLoc 1 510 0
, Jmp ".L63"
, Mark ".L65"
, PLoc 1 511 0
, Mark ".L66"
, PLoc 1 505 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "B" )
, J CEQ ".L284" "A"
    ( Literal 0 )
, Jmp ".L285"
, Mark ".L284"
, PLoc 1 511 0
, Jmp ".L68"
, Mark ".L285"
, PLoc 1 505 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
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
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
, Load "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Store "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, PLoc 1 511 0
, Mark ".L67"
, PLoc 1 505 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
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
    ( Literal 3 )
, Store "A"
    ( Variable "B" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, PLoc 1 511 0
, PLoc 1 505 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
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
    ( Literal 2 )
, Store "A"
    ( Variable "B" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, PLoc 1 511 0
, Jmp ".L66"
, Mark ".L68"
, PLoc 1 513 0
, PLoc 1 509 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, PText
, Mark "strcpy"
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
, PLoc 1 521 0
, PLoc 1 517 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "SP" )
, PLoc 1 513 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Store "A"
    ( Variable "B" )
, PLoc 1 518 0
, Mark ".L69"
, PLoc 1 513 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "B" )
, J CEQ ".L286" "A"
    ( Literal 0 )
, Jmp ".L287"
, Mark ".L286"
, PLoc 1 518 0
, Jmp ".L71"
, Mark ".L287"
, PLoc 1 513 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
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
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
, Load "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Store "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, PLoc 1 518 0
, Mark ".L70"
, PLoc 1 513 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
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
    ( Literal 3 )
, Store "A"
    ( Variable "B" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, PLoc 1 518 0
, PLoc 1 513 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
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
    ( Literal 2 )
, Store "A"
    ( Variable "B" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, PLoc 1 518 0
, Jmp ".L69"
, Mark ".L71"
, PLoc 1 520 0
, Mov "A"
    ( Literal 0 )
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
, PLoc 1 513 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
, Load "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Store "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, PLoc 1 521 0
, PLoc 1 517 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, PText
, Mark "strcmp"
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
, PLoc 1 531 0
, PLoc 1 528 0
, Mark ".L72"
, PLoc 1 525 0
, PLoc 1 521 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Literal 1 )
, J CNE ".L288" "A"
    ( Literal 0 )
, PLoc 1 525 0
, PLoc 1 521 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, L CNE "B"
    ( Literal 0 )
, Mark ".L288"
, Mov "A"
    ( Variable "B" )
, J CEQ ".L289" "A"
    ( Literal 0 )
, Jmp ".L290"
, Mark ".L289"
, PLoc 1 528 0
, Jmp ".L74"
, Mark ".L290"
, PLoc 1 526 0
, PLoc 1 521 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
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
, PLoc 1 526 0
, PLoc 1 521 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CLT "A"
    ( Variable "B" )
, J CEQ ".L291" "A"
    ( Literal 0 )
, PLoc 1 526 0
, Mov "A"
    ( Literal 0 )
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
, Mov "A"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Sub "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, Mark ".L291"
, PLoc 1 528 0
, PLoc 1 521 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
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
, PLoc 1 528 0
, PLoc 1 521 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CLT "A"
    ( Variable "B" )
, J CEQ ".L292" "A"
    ( Literal 0 )
, PLoc 1 528 0
, Mov "A"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, Mark ".L292"
, Mark ".L73"
, PLoc 1 525 0
, PLoc 1 521 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
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
    ( Literal 2 )
, Store "A"
    ( Variable "B" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, PLoc 1 525 0
, PLoc 1 521 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
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
    ( Literal 3 )
, Store "A"
    ( Variable "B" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, PLoc 1 528 0
, Jmp ".L72"
, Mark ".L74"
, PLoc 1 531 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "A" )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, PText
, Mark "strchr"
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
, PLoc 1 539 0
, PLoc 1 536 0
, Mark ".L75"
, PLoc 1 535 0
, PLoc 1 531 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "B" )
, J CEQ ".L293" "A"
    ( Literal 0 )
, Jmp ".L294"
, Mark ".L293"
, PLoc 1 536 0
, Jmp ".L77"
, Mark ".L294"
, PLoc 1 531 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
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
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CEQ "A"
    ( Variable "B" )
, J CEQ ".L295" "A"
    ( Literal 0 )
, PLoc 1 536 0
, PLoc 1 531 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, Mark ".L295"
, PLoc 1 536 0
, Mark ".L76"
, PLoc 1 535 0
, PLoc 1 531 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
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
    ( Literal 2 )
, Store "A"
    ( Variable "B" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, PLoc 1 536 0
, Jmp ".L75"
, Mark ".L77"
, PLoc 1 539 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "A" )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, PText
, Mark "strdup"
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
    ( Literal 2 )
, PLoc 1 546 0
, PLoc 1 543 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "SP" )
, PLoc 1 539 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
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
, Mov "A"
    ( Variable ".L296" )
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
, Jmp "strlen"
, Mark ".L296"
, Mov "A"
    ( Variable "B" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Store "A"
    ( Variable "B" )
, PLoc 1 544 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "SP" )
, PLoc 1 543 0
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
, PLoc 1 544 0
, Mov "A"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
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
, Mov "A"
    ( Variable ".L297" )
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
, Jmp "malloc"
, Mark ".L297"
, Mov "A"
    ( Variable "B" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
, Store "A"
    ( Variable "B" )
, PLoc 1 545 0
, PLoc 1 539 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
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
, PLoc 1 544 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
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
, Mov "A"
    ( Variable ".L298" )
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
, Jmp "strcpy"
, Mark ".L298"
, Mov "A"
    ( Variable "B" )
, Add "SP"
    ( Literal 2 )
, PLoc 1 546 0
, PLoc 1 544 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, PText
, Mark "strtok_r"
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
    ( Literal 4 )
, PLoc 1 590 0
, PLoc 1 553 0
, PLoc 1 554 0
, PLoc 1 555 0
, PLoc 1 557 0
, PLoc 1 546 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
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
, PLoc 1 557 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CEQ "A"
    ( Variable "B" )
, Mov "B"
    ( Literal 0 )
, J CEQ ".L299" "A"
    ( Literal 0 )
, PLoc 1 546 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 4 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
, Store "A"
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
, PLoc 1 557 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CEQ "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, L CNE "B"
    ( Literal 0 )
, Mark ".L299"
, Mov "A"
    ( Variable "B" )
, J CEQ ".L300" "A"
    ( Literal 0 )
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "A" )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, Mark ".L300"
, PLoc 1 563 0
, Mark ".L86"
, PLoc 1 546 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
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
    ( Literal 2 )
, Store "A"
    ( Variable "B" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
, Store "A"
    ( Variable "B" )
, PLoc 1 566 0
, PLoc 1 565 0
, PLoc 1 546 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Store "A"
    ( Variable "B" )
, PLoc 1 566 0
, Mark ".L78"
, PLoc 1 565 0
, PLoc 1 553 0
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
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777213 )
, Store "A"
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
, PLoc 1 565 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CNE "A"
    ( Variable "B" )
, J CEQ ".L301" "A"
    ( Literal 0 )
, Jmp ".L302"
, Mark ".L301"
, PLoc 1 566 0
, Jmp ".L80"
, Mark ".L302"
, PLoc 1 554 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
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
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777213 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CEQ "A"
    ( Variable "B" )
, J CEQ ".L303" "A"
    ( Literal 0 )
, PLoc 1 566 0
, Jmp ".L86"
, Mark ".L303"
, Mark ".L79"
, Jmp ".L78"
, Mark ".L80"
, PLoc 1 572 0
, PLoc 1 570 0
, PLoc 1 554 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
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
, PLoc 1 570 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CEQ "A"
    ( Variable "B" )
, J CEQ ".L304" "A"
    ( Literal 0 )
, PLoc 1 572 0
, PLoc 1 571 0
, Mov "A"
    ( Literal 0 )
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
, PLoc 1 546 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 4 )
, Load "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Store "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, PLoc 1 572 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "A" )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, Mark ".L304"
, PLoc 1 574 0
, PLoc 1 546 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
, Load "A"
    ( Variable "B" )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, PLoc 1 574 0
, Mov "A"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Sub "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777212 )
, Store "A"
    ( Variable "B" )
, PLoc 1 590 0
, Mark ".L81"
, PLoc 1 581 0
, PLoc 1 546 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
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
    ( Literal 2 )
, Store "A"
    ( Variable "B" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
, Store "A"
    ( Variable "B" )
, PLoc 1 582 0
, PLoc 1 546 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Store "A"
    ( Variable "B" )
, PLoc 1 590 0
, Mark ".L84"
, PLoc 1 584 0
, PLoc 1 553 0
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
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777213 )
, Store "A"
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
, PLoc 1 554 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CEQ "A"
    ( Variable "B" )
, J CEQ ".L305" "A"
    ( Literal 0 )
, PLoc 1 590 0
, PLoc 1 585 0
, PLoc 1 554 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
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
, PLoc 1 585 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CEQ "A"
    ( Variable "B" )
, J CEQ ".L306" "A"
    ( Literal 0 )
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
, Store "A"
    ( Variable "B" )
, Jmp ".L307"
, Mark ".L306"
, Mov "A"
    ( Literal 0 )
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
, PLoc 1 546 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
, Load "A"
    ( Variable "B" )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, PLoc 1 585 0
, Mov "A"
    ( Literal 0 )
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
, Mov "A"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Sub "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Store "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mark ".L307"
, PLoc 1 589 0
, PLoc 1 546 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
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
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 4 )
, Load "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Store "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, PLoc 1 590 0
, PLoc 1 555 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777212 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, Mark ".L305"
, PLoc 1 590 0
, PLoc 1 554 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777213 )
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
, PLoc 1 590 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CNE "A"
    ( Variable "B" )
, J CEQ ".L308" "A"
    ( Literal 0 )
, Jmp ".L84"
, Mark ".L308"
, Mark ".L85"
, Mark ".L82"
, Jmp ".L81"
, Mark ".L83"
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, PData
    ( Just 0 )
, Mark "charmap"
, PLong 0
, PLong 1
, PLong 2
, PLong 3
, PLong 4
, PLong 5
, PLong 6
, PLong 7
, PLong 8
, PLong 9
, PLong 10
, PLong 11
, PLong 12
, PLong 13
, PLong 14
, PLong 15
, PLong 16
, PLong 17
, PLong 18
, PLong 19
, PLong 20
, PLong 21
, PLong 22
, PLong 23
, PLong 24
, PLong 25
, PLong 26
, PLong 27
, PLong 28
, PLong 29
, PLong 30
, PLong 31
, PLong 32
, PLong 33
, PLong 34
, PLong 35
, PLong 36
, PLong 37
, PLong 38
, PLong 39
, PLong 40
, PLong 41
, PLong 42
, PLong 43
, PLong 44
, PLong 45
, PLong 46
, PLong 47
, PLong 48
, PLong 49
, PLong 50
, PLong 51
, PLong 52
, PLong 53
, PLong 54
, PLong 55
, PLong 56
, PLong 57
, PLong 58
, PLong 59
, PLong 60
, PLong 61
, PLong 62
, PLong 63
, PLong 64
, PLong 97
, PLong 98
, PLong 99
, PLong 100
, PLong 101
, PLong 102
, PLong 103
, PLong 104
, PLong 105
, PLong 106
, PLong 107
, PLong 108
, PLong 109
, PLong 110
, PLong 111
, PLong 112
, PLong 113
, PLong 114
, PLong 115
, PLong 116
, PLong 117
, PLong 118
, PLong 119
, PLong 120
, PLong 121
, PLong 122
, PLong 91
, PLong 92
, PLong 93
, PLong 94
, PLong 95
, PLong 96
, PLong 97
, PLong 98
, PLong 99
, PLong 100
, PLong 101
, PLong 102
, PLong 103
, PLong 104
, PLong 105
, PLong 106
, PLong 107
, PLong 108
, PLong 109
, PLong 110
, PLong 111
, PLong 112
, PLong 113
, PLong 114
, PLong 115
, PLong 116
, PLong 117
, PLong 118
, PLong 119
, PLong 120
, PLong 121
, PLong 122
, PLong 123
, PLong 124
, PLong 125
, PLong 126
, PLong 127
, PLong 128
, PLong 129
, PLong 130
, PLong 131
, PLong 132
, PLong 133
, PLong 134
, PLong 135
, PLong 136
, PLong 137
, PLong 138
, PLong 139
, PLong 140
, PLong 141
, PLong 142
, PLong 143
, PLong 144
, PLong 145
, PLong 146
, PLong 147
, PLong 148
, PLong 149
, PLong 150
, PLong 151
, PLong 152
, PLong 153
, PLong 154
, PLong 155
, PLong 156
, PLong 157
, PLong 158
, PLong 159
, PLong 160
, PLong 161
, PLong 162
, PLong 163
, PLong 164
, PLong 165
, PLong 166
, PLong 167
, PLong 168
, PLong 169
, PLong 170
, PLong 171
, PLong 172
, PLong 173
, PLong 174
, PLong 175
, PLong 176
, PLong 177
, PLong 178
, PLong 179
, PLong 180
, PLong 181
, PLong 182
, PLong 183
, PLong 184
, PLong 185
, PLong 186
, PLong 187
, PLong 188
, PLong 189
, PLong 190
, PLong 191
, PLong 192
, PLong 193
, PLong 194
, PLong 195
, PLong 196
, PLong 197
, PLong 198
, PLong 199
, PLong 200
, PLong 201
, PLong 202
, PLong 203
, PLong 204
, PLong 205
, PLong 206
, PLong 207
, PLong 208
, PLong 209
, PLong 210
, PLong 211
, PLong 212
, PLong 213
, PLong 214
, PLong 215
, PLong 216
, PLong 217
, PLong 218
, PLong 219
, PLong 220
, PLong 221
, PLong 222
, PLong 223
, PLong 224
, PLong 225
, PLong 226
, PLong 227
, PLong 228
, PLong 229
, PLong 230
, PLong 231
, PLong 232
, PLong 233
, PLong 234
, PLong 235
, PLong 236
, PLong 237
, PLong 238
, PLong 239
, PLong 240
, PLong 241
, PLong 242
, PLong 243
, PLong 244
, PLong 245
, PLong 246
, PLong 247
, PLong 248
, PLong 249
, PLong 250
, PLong 251
, PLong 252
, PLong 253
, PLong 254
, PLong 255
, PText
, Mark "strcasecmp"
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
    ( Literal 3 )
, PLoc 1 649 0
, PLoc 1 642 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "SP" )
, PLoc 1 590 0
, Mov "A"
    ( Variable "charmap" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Store "A"
    ( Variable "B" )
, PLoc 1 643 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "SP" )
, PLoc 1 590 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
, Store "A"
    ( Variable "B" )
, PLoc 1 644 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "SP" )
, PLoc 1 590 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777213 )
, Store "A"
    ( Variable "B" )
, PLoc 1 646 0
, Mark ".L87"
, PLoc 1 642 0
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
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, PLoc 1 646 0
, PLoc 1 643 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
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
, PLoc 1 646 0
, PLoc 1 642 0
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
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, PLoc 1 646 0
, PLoc 1 644 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777213 )
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
    ( Literal 16777213 )
, Store "A"
    ( Variable "B" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CEQ "A"
    ( Variable "B" )
, J CEQ ".L309" "A"
    ( Literal 0 )
, PLoc 1 646 0
, PLoc 1 643 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
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
    ( Literal 16777214 )
, Store "A"
    ( Variable "B" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
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
, PLoc 1 646 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CEQ "A"
    ( Variable "B" )
, J CEQ ".L310" "A"
    ( Literal 0 )
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "A" )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, Mark ".L310"
, Jmp ".L311"
, Mark ".L309"
, Jmp ".L88"
, Mark ".L311"
, Jmp ".L87"
, Mark ".L88"
, PLoc 1 649 0
, PLoc 1 642 0
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
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, PLoc 1 649 0
, PLoc 1 643 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
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
, PLoc 1 649 0
, PLoc 1 642 0
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
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, PLoc 1 649 0
, PLoc 1 644 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777213 )
, Load "A"
    ( Variable "B" )
, Sub "A"
    ( Literal 1 )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777213 )
, Store "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Sub "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, PText
, Mark "strncasecmp"
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
    ( Literal 3 )
, PLoc 1 668 0
, PLoc 1 664 0
, PLoc 1 656 0
, PLoc 1 649 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 4 )
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
, PLoc 1 656 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CNE "A"
    ( Variable "B" )
, J CEQ ".L312" "A"
    ( Literal 0 )
, PLoc 1 664 0
, PLoc 1 657 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "SP" )
, PLoc 1 590 0
, Mov "A"
    ( Variable "charmap" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Store "A"
    ( Variable "B" )
, PLoc 1 658 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "SP" )
, PLoc 1 649 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
, Store "A"
    ( Variable "B" )
, PLoc 1 659 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "SP" )
, PLoc 1 649 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777213 )
, Store "A"
    ( Variable "B" )
, PLoc 1 664 0
, Mark ".L89"
, PLoc 1 662 0
, PLoc 1 657 0
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
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, PLoc 1 662 0
, PLoc 1 658 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
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
, PLoc 1 662 0
, PLoc 1 657 0
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
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, PLoc 1 662 0
, PLoc 1 659 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777213 )
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
    ( Literal 16777213 )
, Store "A"
    ( Variable "B" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CNE "A"
    ( Variable "B" )
, J CEQ ".L313" "A"
    ( Literal 0 )
, PLoc 1 662 0
, PLoc 1 657 0
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
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, PLoc 1 662 0
, PLoc 1 658 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
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
, PLoc 1 662 0
, PLoc 1 657 0
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
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, PLoc 1 662 0
, PLoc 1 659 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777213 )
, Load "A"
    ( Variable "B" )
, Sub "A"
    ( Literal 1 )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777213 )
, Store "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Sub "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, Mark ".L313"
, PLoc 1 664 0
, PLoc 1 658 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
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
    ( Literal 16777214 )
, Store "A"
    ( Variable "B" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
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
, PLoc 1 664 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CEQ "A"
    ( Variable "B" )
, J CEQ ".L314" "A"
    ( Literal 0 )
, Jmp ".L90"
, Mark ".L314"
, PLoc 1 649 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 4 )
, Load "A"
    ( Variable "B" )
, Sub "A"
    ( Literal 1 )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 4 )
, Store "A"
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
, PLoc 1 664 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CNE "A"
    ( Variable "B" )
, J CEQ ".L315" "A"
    ( Literal 0 )
, Jmp ".L89"
, Mark ".L315"
, Mark ".L90"
, Mark ".L312"
, PLoc 1 668 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "A" )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, PText
, Mark "strpbrk"
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
    ( Literal 3 )
, PLoc 1 683 0
, PLoc 1 675 0
, PLoc 1 676 0
, PLoc 1 679 0
, Mark ".L91"
, PLoc 1 678 0
, PLoc 1 668 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
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
    ( Literal 2 )
, Store "A"
    ( Variable "B" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
, Store "A"
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
, PLoc 1 678 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CNE "A"
    ( Variable "B" )
, J CEQ ".L316" "A"
    ( Literal 0 )
, PLoc 1 679 0
, PLoc 1 668 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Store "A"
    ( Variable "B" )
, PLoc 1 679 0
, Mark ".L93"
, PLoc 1 675 0
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
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777213 )
, Store "A"
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
, PLoc 1 679 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CNE "A"
    ( Variable "B" )
, J CEQ ".L317" "A"
    ( Literal 0 )
, Jmp ".L318"
, Mark ".L317"
, Jmp ".L95"
, Mark ".L318"
, PLoc 1 676 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777213 )
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
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CEQ "A"
    ( Variable "B" )
, J CEQ ".L319" "A"
    ( Literal 0 )
, PLoc 1 679 0
, PLoc 1 668 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
, Load "A"
    ( Variable "B" )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, PLoc 1 679 0
, Mov "A"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Sub "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Mov "B"
    ( Variable "A" )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, Mark ".L319"
, Mark ".L94"
, Jmp ".L93"
, Mark ".L95"
, Jmp ".L320"
, Mark ".L316"
, Jmp ".L92"
, Mark ".L320"
, Jmp ".L91"
, Mark ".L92"
, PLoc 1 683 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "A" )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, PText
, Mark "puts"
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
, PLoc 1 700 0
, PLoc 1 699 0
, PLoc 1 683 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
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
, Mov "A"
    ( Variable ".L321" )
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
, Jmp "print_str"
, Mark ".L321"
, Mov "A"
    ( Variable "B" )
, Add "SP"
    ( Literal 1 )
, PLoc 1 700 0
, Mov "A"
    ( Literal 10 )
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
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, PText
, Mark "vsnprintf"
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
    ( Literal 38 )
, PLoc 1 767 0
, PLoc 1 704 0
, PLoc 1 705 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "SP" )
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
, Store "A"
    ( Variable "B" )
, PLoc 1 706 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "SP" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777213 )
, Mov "A"
    ( Literal 0 )
, Store "A"
    ( Variable "B" )
, PLoc 1 764 0
, PLoc 1 707 0
, PLoc 1 700 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 4 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Store "A"
    ( Variable "B" )
, PLoc 1 764 0
, Mark ".L96"
, PLoc 1 707 0
, PLoc 1 704 0
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
, J CEQ ".L322" "A"
    ( Literal 0 )
, Jmp ".L323"
, Mark ".L322"
, PLoc 1 764 0
, Jmp ".L98"
, Mark ".L323"
, PLoc 1 718 0
, PLoc 1 708 0
, PLoc 1 704 0
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
, PLoc 1 708 0
, Mov "A"
    ( Literal 37 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CNE "A"
    ( Variable "B" )
, J CEQ ".L324" "A"
    ( Literal 0 )
, PLoc 1 718 0
, PLoc 1 714 0
, PLoc 1 709 0
, PLoc 1 706 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777213 )
, Load "A"
    ( Variable "B" )
, L CEQ "A"
    ( Literal 0 )
, J CEQ ".L325" "A"
    ( Literal 0 )
, PLoc 1 714 0
, PLoc 1 710 0
, PLoc 1 700 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
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
, PLoc 1 710 0
, PLoc 1 705 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
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
, PLoc 1 710 0
, Mov "A"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CLE "A"
    ( Variable "B" )
, J CEQ ".L326" "A"
    ( Literal 0 )
, PLoc 1 712 0
, PLoc 1 711 0
, Mov "A"
    ( Literal 1 )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777213 )
, Store "A"
    ( Variable "B" )
, PLoc 1 712 0
, Mov "A"
    ( Literal 0 )
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
, PLoc 1 700 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
, Load "A"
    ( Variable "B" )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, PLoc 1 705 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Store "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp ".L327"
, Mark ".L326"
, PLoc 1 714 0
, PLoc 1 704 0
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
, PLoc 1 714 0
, PLoc 1 700 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
, Load "A"
    ( Variable "B" )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, PLoc 1 705 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Store "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mark ".L327"
, Mark ".L325"
, PLoc 1 717 0
, PLoc 1 705 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
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
    ( Literal 16777214 )
, Store "A"
    ( Variable "B" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, PLoc 1 718 0
, Jmp ".L97"
, Mark ".L324"
, PLoc 1 721 0
, PLoc 1 722 0
, PLoc 1 752 0
, PLoc 1 723 0
, Mark ".L108"
, PLoc 1 752 0
, PLoc 1 723 0
, PLoc 1 704 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Load "A"
    ( Variable "B" )
, Add "A"
    ( Literal 1 )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Store "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777179 )
, Store "A"
    ( Variable "B" )
, PLoc 1 752 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777179 )
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
, Mov "A"
    ( Literal 108 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CEQ "A"
    ( Variable "B" )
, J CEQ ".L328" "A"
    ( Literal 0 )
, Jmp ".L100"
, Mark ".L328"
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777179 )
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
, Mov "A"
    ( Literal 100 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CEQ "A"
    ( Variable "B" )
, J CEQ ".L329" "A"
    ( Literal 0 )
, Jmp ".L101"
, Mark ".L329"
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777179 )
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
, Mov "A"
    ( Literal 117 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CEQ "A"
    ( Variable "B" )
, J CEQ ".L330" "A"
    ( Literal 0 )
, Jmp ".L102"
, Mark ".L330"
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777179 )
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
, Mov "A"
    ( Literal 120 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CEQ "A"
    ( Variable "B" )
, J CEQ ".L331" "A"
    ( Literal 0 )
, Jmp ".L103"
, Mark ".L331"
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777179 )
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
, Mov "A"
    ( Literal 115 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CEQ "A"
    ( Variable "B" )
, J CEQ ".L332" "A"
    ( Literal 0 )
, Jmp ".L104"
, Mark ".L332"
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777179 )
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
, Mov "A"
    ( Literal 99 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CEQ "A"
    ( Variable "B" )
, J CEQ ".L333" "A"
    ( Literal 0 )
, Jmp ".L105"
, Mark ".L333"
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777179 )
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
, Mov "A"
    ( Literal 37 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CEQ "A"
    ( Variable "B" )
, J CEQ ".L334" "A"
    ( Literal 0 )
, Jmp ".L106"
, Mark ".L334"
, Jmp ".L107"
, PLoc 1 725 0
, Mark ".L100"
, Jmp ".L108"
, PLoc 1 727 0
, Mark ".L101"
, Mark ".L102"
, PLoc 1 721 0
, Mov "A"
    ( Variable "BP" )
, Add "A"
    ( Literal 16777181 )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, PLoc 1 727 0
, Mov "A"
    ( Literal 32 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, Mov "A"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Sub "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
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
, PLoc 1 700 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 5 )
, Load "A"
    ( Variable "B" )
, Add "A"
    ( Literal 1 )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 5 )
, Store "A"
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
, Mov "A"
    ( Variable ".L335" )
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
, Jmp "stringify_int"
, Mark ".L335"
, Mov "A"
    ( Variable "B" )
, Add "SP"
    ( Literal 2 )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777180 )
, Store "A"
    ( Variable "B" )
, PLoc 1 730 0
, Jmp ".L99"
, PLoc 1 731 0
, Mark ".L103"
, PLoc 1 721 0
, Mov "A"
    ( Variable "BP" )
, Add "A"
    ( Literal 16777181 )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, PLoc 1 731 0
, Mov "A"
    ( Literal 32 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, Mov "A"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Sub "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
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
, PLoc 1 700 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 5 )
, Load "A"
    ( Variable "B" )
, Add "A"
    ( Literal 1 )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 5 )
, Store "A"
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
, Mov "A"
    ( Variable ".L336" )
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
, Jmp "stringify_hex"
, Mark ".L336"
, Mov "A"
    ( Variable "B" )
, Add "SP"
    ( Literal 2 )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777180 )
, Store "A"
    ( Variable "B" )
, PLoc 1 733 0
, Jmp ".L99"
, PLoc 1 734 0
, Mark ".L104"
, PLoc 1 700 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 5 )
, Load "A"
    ( Variable "B" )
, Add "A"
    ( Literal 1 )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 5 )
, Store "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777180 )
, Store "A"
    ( Variable "B" )
, PLoc 1 736 0
, Jmp ".L99"
, PLoc 1 737 0
, Mark ".L105"
, PLoc 1 700 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 5 )
, Load "A"
    ( Variable "B" )
, Add "A"
    ( Literal 1 )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 5 )
, Store "A"
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
, PLoc 1 737 0
, PLoc 1 721 0
, Mov "A"
    ( Variable "BP" )
, Add "A"
    ( Literal 16777181 )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, PLoc 1 737 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Store "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, PLoc 1 739 0
, Mov "A"
    ( Literal 0 )
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
, PLoc 1 721 0
, Mov "A"
    ( Variable "BP" )
, Add "A"
    ( Literal 16777181 )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, PLoc 1 739 0
, Mov "A"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Store "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, PLoc 1 740 0
, PLoc 1 721 0
, Mov "A"
    ( Variable "BP" )
, Add "A"
    ( Literal 16777181 )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777180 )
, Store "A"
    ( Variable "B" )
, PLoc 1 741 0
, Jmp ".L99"
, PLoc 1 742 0
, Mark ".L106"
, Mov "A"
    ( Literal 37 )
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
, PLoc 1 721 0
, Mov "A"
    ( Variable "BP" )
, Add "A"
    ( Literal 16777181 )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, PLoc 1 742 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Store "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, PLoc 1 744 0
, Mov "A"
    ( Literal 0 )
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
, PLoc 1 721 0
, Mov "A"
    ( Variable "BP" )
, Add "A"
    ( Literal 16777181 )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, PLoc 1 744 0
, Mov "A"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Store "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, PLoc 1 745 0
, PLoc 1 721 0
, Mov "A"
    ( Variable "BP" )
, Add "A"
    ( Literal 16777181 )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777180 )
, Store "A"
    ( Variable "B" )
, PLoc 1 746 0
, Jmp ".L99"
, PLoc 1 747 0
, Mark ".L107"
, PLoc 1 704 0
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
, Mov "A"
    ( Variable ".L337" )
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
, Jmp "print_int"
, Mark ".L337"
, Mov "A"
    ( Variable "B" )
, Add "SP"
    ( Literal 1 )
, PLoc 1 749 0
, PData Nothing
, Mark ".L338"
, PString " in "
, PText
, Mov "A"
    ( Variable ".L338" )
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
, Mov "A"
    ( Variable ".L339" )
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
, Jmp "print_str"
, Mark ".L339"
, Mov "A"
    ( Variable "B" )
, Add "SP"
    ( Literal 1 )
, PLoc 1 750 0
, PLoc 1 700 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 4 )
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
, Mov "A"
    ( Variable ".L340" )
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
, Jmp "print_str"
, Mark ".L340"
, Mov "A"
    ( Variable "B" )
, Add "SP"
    ( Literal 1 )
, PLoc 1 751 0
, PData Nothing
, Mark ".L341"
, PString ": unknown format!\n"
, PText
, Mov "A"
    ( Variable ".L341" )
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
, Mov "A"
    ( Variable ".L342" )
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
, Jmp "print_str"
, Mark ".L342"
, Mov "A"
    ( Variable "B" )
, Add "SP"
    ( Literal 1 )
, PLoc 1 752 0
, Mov "A"
    ( Literal 1 )
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
, Exit
, Add "SP"
    ( Literal 1 )
, Mark ".L99"
, PLoc 1 755 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "SP" )
, PLoc 1 722 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777180 )
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
, Mov "A"
    ( Variable ".L343" )
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
, Jmp "strlen"
, Mark ".L343"
, Mov "A"
    ( Variable "B" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777178 )
, Store "A"
    ( Variable "B" )
, PLoc 1 761 0
, PLoc 1 756 0
, PLoc 1 706 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777213 )
, Load "A"
    ( Variable "B" )
, L CEQ "A"
    ( Literal 0 )
, J CEQ ".L344" "A"
    ( Literal 0 )
, PLoc 1 761 0
, PLoc 1 757 0
, PLoc 1 700 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
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
, PLoc 1 757 0
, PLoc 1 705 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
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
, PLoc 1 755 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777178 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CLE "A"
    ( Variable "B" )
, J CEQ ".L345" "A"
    ( Literal 0 )
, PLoc 1 759 0
, PLoc 1 758 0
, Mov "A"
    ( Literal 1 )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777213 )
, Store "A"
    ( Variable "B" )
, PLoc 1 759 0
, Mov "A"
    ( Literal 0 )
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
, PLoc 1 700 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
, Load "A"
    ( Variable "B" )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, PLoc 1 705 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Store "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp ".L346"
, Mark ".L345"
, PLoc 1 761 0
, PLoc 1 722 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777180 )
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
, PLoc 1 761 0
, PLoc 1 700 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
, Load "A"
    ( Variable "B" )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, PLoc 1 705 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
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
, Mov "A"
    ( Variable ".L347" )
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
, Jmp "strcpy"
, Mark ".L347"
, Mov "A"
    ( Variable "B" )
, Add "SP"
    ( Literal 2 )
, Mark ".L346"
, Mark ".L344"
, PLoc 1 764 0
, PLoc 1 705 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
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
, PLoc 1 755 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777178 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
, Store "A"
    ( Variable "B" )
, PLoc 1 764 0
, Mark ".L97"
, PLoc 1 707 0
, PLoc 1 704 0
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
, PLoc 1 764 0
, Jmp ".L96"
, Mark ".L98"
, PLoc 1 766 0
, Mov "A"
    ( Literal 0 )
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
, PLoc 1 700 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
, Load "A"
    ( Variable "B" )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, PLoc 1 705 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Store "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, PLoc 1 767 0
, PLoc 1 705 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, PText
, Mark "vsprintf"
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
, PLoc 1 771 0
, PLoc 1 767 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 4 )
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
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
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
, PLoc 1 771 0
, Mov "A"
    ( Literal 256 )
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
, PLoc 1 767 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
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
, Mov "A"
    ( Variable ".L348" )
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
, Jmp "vsnprintf"
, Mark ".L348"
, Mov "A"
    ( Variable "B" )
, Add "SP"
    ( Literal 4 )
, Mov "B"
    ( Variable "A" )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, PText
, Mark "snprintf"
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
    ( Literal 2 )
, PLoc 1 779 0
, PLoc 1 775 0
, PLoc 1 776 0
, Mov "A"
    ( Variable "BP" )
, Add "A"
    ( Literal 4 )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Store "A"
    ( Variable "B" )
, PLoc 1 777 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "SP" )
, PLoc 1 775 0
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
, PLoc 1 771 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 4 )
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
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
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
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
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
, Mov "A"
    ( Variable ".L349" )
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
, Jmp "vsnprintf"
, Mark ".L349"
, Mov "A"
    ( Variable "B" )
, Add "SP"
    ( Literal 4 )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
, Store "A"
    ( Variable "B" )
, PLoc 1 779 0
, PLoc 1 777 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, PText
, Mark "sprintf"
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
    ( Literal 2 )
, PLoc 1 787 0
, PLoc 1 783 0
, PLoc 1 784 0
, Mov "A"
    ( Variable "BP" )
, Add "A"
    ( Literal 3 )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Store "A"
    ( Variable "B" )
, PLoc 1 785 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "SP" )
, PLoc 1 783 0
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
, PLoc 1 779 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
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
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
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
, Mov "A"
    ( Variable ".L350" )
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
, Jmp "vsprintf"
, Mark ".L350"
, Mov "A"
    ( Variable "B" )
, Add "SP"
    ( Literal 3 )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
, Store "A"
    ( Variable "B" )
, PLoc 1 787 0
, PLoc 1 785 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, PText
, Mark "vprintf"
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
    ( Literal 257 )
, PLoc 1 795 0
, PLoc 1 791 0
, PLoc 1 792 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "SP" )
, PLoc 1 787 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
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
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
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
, PLoc 1 792 0
, Mov "A"
    ( Literal 256 )
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
, PLoc 1 791 0
, Mov "A"
    ( Variable "BP" )
, Add "A"
    ( Literal 16776960 )
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
, Mov "A"
    ( Variable ".L351" )
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
, Jmp "vsnprintf"
, Mark ".L351"
, Mov "A"
    ( Variable "B" )
, Add "SP"
    ( Literal 4 )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16776959 )
, Store "A"
    ( Variable "B" )
, PLoc 1 793 0
, Mov "A"
    ( Literal 0 )
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
, PLoc 1 791 0
, Mov "A"
    ( Variable "BP" )
, Add "A"
    ( Literal 16776960 )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, PLoc 1 792 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16776959 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Store "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, PLoc 1 794 0
, PLoc 1 791 0
, Mov "A"
    ( Variable "BP" )
, Add "A"
    ( Literal 16776960 )
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
, Mov "A"
    ( Variable ".L352" )
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
, Jmp "print_str"
, Mark ".L352"
, Mov "A"
    ( Variable "B" )
, Add "SP"
    ( Literal 1 )
, PLoc 1 795 0
, PLoc 1 792 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16776959 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, PText
, Mark "printf"
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
    ( Literal 2 )
, PLoc 1 803 0
, PLoc 1 799 0
, PLoc 1 800 0
, Mov "A"
    ( Variable "BP" )
, Add "A"
    ( Literal 2 )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Store "A"
    ( Variable "B" )
, PLoc 1 801 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "SP" )
, PLoc 1 799 0
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
, PLoc 1 795 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
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
, Mov "A"
    ( Variable ".L353" )
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
, Jmp "vprintf"
, Mark ".L353"
, Mov "A"
    ( Variable "B" )
, Add "SP"
    ( Literal 2 )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
, Store "A"
    ( Variable "B" )
, PLoc 1 803 0
, PLoc 1 801 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, PData
    ( Just 0 )
, Mark "stdin"
, PLong 1
, PData
    ( Just 0 )
, Mark "stdout"
, PLong 1
, PData
    ( Just 0 )
, Mark "stderr"
, PLong 1
, PText
, Mark "fprintf"
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
, PLoc 1 815 0
, PLoc 1 812 0
, PLoc 1 813 0
, Mov "A"
    ( Variable "BP" )
, Add "A"
    ( Literal 3 )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Store "A"
    ( Variable "B" )
, PLoc 1 814 0
, PLoc 1 812 0
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
, PLoc 1 803 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
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
, Mov "A"
    ( Variable ".L354" )
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
, Jmp "vprintf"
, Mark ".L354"
, Mov "A"
    ( Variable "B" )
, Add "SP"
    ( Literal 2 )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, PText
, Mark "vfprintf"
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
, PLoc 1 819 0
, PLoc 1 815 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 4 )
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
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
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
, Mov "A"
    ( Variable ".L355" )
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
, Jmp "vprintf"
, Mark ".L355"
, Mov "A"
    ( Variable "B" )
, Add "SP"
    ( Literal 2 )
, Mov "B"
    ( Variable "A" )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, PText
, Mark "fileno"
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
, PLoc 1 823 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "A" )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, PText
, Mark "fopen"
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
, PLoc 1 827 0
, PLoc 1 803 0
, Mov "B"
    ( Variable "stdin" )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, PText
, Mark "fclose"
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
, PLoc 1 831 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "A" )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, PText
, Mark "fwrite"
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
    ( Literal 3 )
, PLoc 1 839 0
, PLoc 1 835 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "SP" )
, PLoc 1 831 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Store "A"
    ( Variable "B" )
, PLoc 1 836 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "SP" )
, PLoc 1 831 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
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
, PLoc 1 836 0
, PLoc 1 831 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 4 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, Mov "A"
    ( Variable ".L356" )
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
, Jmp "__builtin_mul"
, Mark ".L356"
, Mov "A"
    ( Variable "B" )
, Add "SP"
    ( Literal 2 )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
, Store "A"
    ( Variable "B" )
, PLoc 1 837 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "SP" )
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777213 )
, Store "A"
    ( Variable "B" )
, Mark ".L109"
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777213 )
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
, PLoc 1 836 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CLT "A"
    ( Variable "B" )
, J CEQ ".L357" "A"
    ( Literal 0 )
, Jmp ".L358"
, Mark ".L357"
, PLoc 1 837 0
, Jmp ".L111"
, Mark ".L358"
, PLoc 1 835 0
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
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, PLoc 1 837 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777213 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
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
, Mark ".L110"
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777213 )
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
    ( Literal 16777213 )
, Store "A"
    ( Variable "B" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp ".L109"
, Mark ".L111"
, PLoc 1 839 0
, PLoc 1 836 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, PText
, Mark "fputs"
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
, PLoc 1 843 0
, PLoc 1 839 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
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
, Mov "A"
    ( Variable ".L359" )
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
, Jmp "print_str"
, Mark ".L359"
, Mov "A"
    ( Variable "B" )
, Add "SP"
    ( Literal 1 )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, PText
, Mark "fgets"
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
    ( Literal 2 )
, PLoc 1 856 0
, PLoc 1 852 0
, PLoc 1 847 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "SP" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Mov "A"
    ( Literal 0 )
, Store "A"
    ( Variable "B" )
, PLoc 1 852 0
, Mark ".L112"
, PLoc 1 847 0
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
, PLoc 1 843 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
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
, PLoc 1 847 0
, Mov "A"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Sub "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CLT "A"
    ( Variable "B" )
, J CEQ ".L360" "A"
    ( Literal 0 )
, Jmp ".L361"
, Mark ".L360"
, PLoc 1 852 0
, Jmp ".L114"
, Mark ".L361"
, PLoc 1 848 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "SP" )
, GetC "A"
, J CNE ".L362" "A"
    ( Literal 0 )
, Mov "A"
    ( Literal
        ( -1 )
    )
, Mark ".L362"
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
, Store "A"
    ( Variable "B" )
, PLoc 1 849 0
, PLoc 1 848 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
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
, PLoc 1 849 0
, PLoc 1 843 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
, Load "A"
    ( Variable "B" )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, PLoc 1 847 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Store "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, PLoc 1 852 0
, PLoc 1 850 0
, PLoc 1 848 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
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
, PLoc 1 850 0
, Mov "A"
    ( Literal 10 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CEQ "A"
    ( Variable "B" )
, Mov "B"
    ( Literal 1 )
, J CNE ".L363" "A"
    ( Literal 0 )
, PLoc 1 848 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777214 )
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
, PLoc 1 850 0
, Mov "A"
    ( Literal 0 )
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
, Mov "A"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Sub "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CEQ "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, L CNE "B"
    ( Literal 0 )
, Mark ".L363"
, Mov "A"
    ( Variable "B" )
, J CEQ ".L364" "A"
    ( Literal 0 )
, PLoc 1 852 0
, PLoc 1 851 0
, Mov "A"
    ( Literal 0 )
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
, PLoc 1 843 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
, Load "A"
    ( Variable "B" )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, PLoc 1 851 0
, PLoc 1 847 0
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
, PLoc 1 851 0
, Mov "A"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Store "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, PLoc 1 852 0
, PLoc 1 847 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, Mark ".L364"
, PLoc 1 852 0
, Mark ".L113"
, PLoc 1 847 0
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
, PLoc 1 852 0
, Jmp ".L112"
, Mark ".L114"
, PLoc 1 855 0
, Mov "A"
    ( Literal 0 )
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
, PLoc 1 843 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
, Load "A"
    ( Variable "B" )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, PLoc 1 855 0
, PLoc 1 843 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
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
, PLoc 1 855 0
, Mov "A"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Sub "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
, Store "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, PLoc 1 856 0
, PLoc 1 843 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 3 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, PData
    ( Just 0 )
, Mark "g_ungot"
, PLong 16777215
, PData Nothing
, Mark "eof_seen"
, PLong 0
, PText
, Mark "fgetc"
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
, PLoc 1 875 0
, PLoc 1 863 0
, PLoc 1 871 0
, PLoc 1 864 0
, PLoc 1 856 0
, Mov "B"
    ( Variable "g_ungot" )
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
, PLoc 1 864 0
, Mov "A"
    ( Literal 0 )
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
, Mov "A"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Sub "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CEQ "A"
    ( Variable "B" )
, J CEQ ".L365" "A"
    ( Literal 0 )
, PLoc 1 871 0
, PLoc 1 867 0
, PLoc 1 856 0
, Mov "B"
    ( Variable "eof_seen" )
, Load "A"
    ( Variable "B" )
, J CEQ ".L366" "A"
    ( Literal 0 )
, PLoc 1 867 0
, Mov "A"
    ( Literal 0 )
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
, Mov "A"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Sub "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, Mark ".L366"
, PLoc 1 869 0
, GetC "A"
, J CNE ".L367" "A"
    ( Literal 0 )
, Mov "A"
    ( Literal
        ( -1 )
    )
, Mark ".L367"
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Store "A"
    ( Variable "B" )
, PLoc 1 870 0
, PLoc 1 863 0
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
, PLoc 1 870 0
, Mov "A"
    ( Literal 0 )
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
, Mov "A"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Sub "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CEQ "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "eof_seen" )
, Store "A"
    ( Variable "B" )
, PLoc 1 871 0
, PLoc 1 863 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, Mark ".L365"
, PLoc 1 873 0
, PLoc 1 856 0
, Mov "B"
    ( Variable "g_ungot" )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Store "A"
    ( Variable "B" )
, PLoc 1 874 0
, Mov "A"
    ( Literal 0 )
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
, Mov "A"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Sub "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "g_ungot" )
, Store "A"
    ( Variable "B" )
, PLoc 1 875 0
, PLoc 1 863 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, PText
, Mark "getc"
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
, PLoc 1 879 0
, PLoc 1 875 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
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
, Mov "A"
    ( Variable ".L368" )
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
, Jmp "fgetc"
, Mark ".L368"
, Mov "A"
    ( Variable "B" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, PText
, Mark "ungetc"
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
, PLoc 1 885 0
, PLoc 1 883 0
, PLoc 1 856 0
, Mov "B"
    ( Variable "g_ungot" )
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
, PLoc 1 883 0
, Mov "A"
    ( Literal 0 )
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
, Mov "A"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Sub "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CEQ "A"
    ( Variable "B" )
, J CEQ ".L369" "A"
    ( Literal 0 )
, PLoc 1 879 0
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 2 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "g_ungot" )
, Store "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, Mark ".L369"
, PLoc 1 885 0
, Mov "A"
    ( Literal 0 )
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
, Mov "A"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Sub "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, Mov "SP"
    ( Variable "BP" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "BP"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Jmp "A"
, PText
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
, PLoc 1 903 0
, PLoc 1 900 0
, PLoc 1 892 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "SP" )
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Mov "A"
    ( Literal 1 )
, Store "A"
    ( Variable "B" )
, PLoc 1 900 0
, Mark ".L115"
, PLoc 1 892 0
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
, Mov "A"
    ( Literal 100 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, L CLE "A"
    ( Variable "B" )
, J CEQ ".L370" "A"
    ( Literal 0 )
, Jmp ".L371"
, Mark ".L370"
, PLoc 1 900 0
, Jmp ".L117"
, Mark ".L371"
, PLoc 1 893 0
, PLoc 1 892 0
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
, PLoc 1 893 0
, Mov "A"
    ( Literal 5 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, Mov "A"
    ( Variable ".L372" )
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
, Jmp "__builtin_mod"
, Mark ".L372"
, Mov "A"
    ( Variable "B" )
, Add "SP"
    ( Literal 2 )
, J CEQ ".L373" "A"
    ( Literal 0 )
, PLoc 1 897 0
, PLoc 1 894 0
, PLoc 1 892 0
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
, PLoc 1 894 0
, Mov "A"
    ( Literal 3 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, Mov "A"
    ( Variable ".L374" )
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
, Jmp "__builtin_mod"
, Mark ".L374"
, Mov "A"
    ( Variable "B" )
, Add "SP"
    ( Literal 2 )
, J CEQ ".L375" "A"
    ( Literal 0 )
, PLoc 1 895 0
, PLoc 1 892 0
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
, PLoc 1 895 0
, PData Nothing
, Mark ".L376"
, PString "%d\n"
, PText
, Mov "A"
    ( Variable ".L376" )
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
, Mov "A"
    ( Variable ".L377" )
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
, Jmp "printf"
, Mark ".L377"
, Mov "A"
    ( Variable "B" )
, Add "SP"
    ( Literal 2 )
, Jmp ".L378"
, Mark ".L375"
, PLoc 1 897 0
, PData Nothing
, Mark ".L379"
, PString "Fizz\n"
, PText
, Mov "A"
    ( Variable ".L379" )
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
, Mov "A"
    ( Variable ".L380" )
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
, Jmp "printf"
, Mark ".L380"
, Mov "A"
    ( Variable "B" )
, Add "SP"
    ( Literal 1 )
, Mark ".L378"
, Jmp ".L381"
, Mark ".L373"
, PLoc 1 900 0
, PData Nothing
, Mark ".L382"
, PString "FizzBuzz\n"
, PText
, Mov "A"
    ( Variable ".L382" )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, PLoc 1 892 0
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
, Mov "B"
    ( Variable "BP" )
, Add "B"
    ( Literal 16777215 )
, Load "A"
    ( Variable "B" )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, Mov "A"
    ( Variable ".L383" )
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
, Jmp "__builtin_mul"
, Mark ".L383"
, Mov "A"
    ( Variable "B" )
, Add "SP"
    ( Literal 2 )
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
, PLoc 1 900 0
, Mov "A"
    ( Literal 3 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, Mov "A"
    ( Variable ".L384" )
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
, Jmp "__builtin_mod"
, Mark ".L384"
, Mov "A"
    ( Variable "B" )
, Add "SP"
    ( Literal 2 )
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
, Mov "A"
    ( Literal 4 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "D"
    ( Variable "SP" )
, Add "D"
    ( Literal
        ( -1 )
    )
, Store "B"
    ( Variable "D" )
, Mov "SP"
    ( Variable "D" )
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
, Mov "A"
    ( Variable ".L385" )
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
, Jmp "__builtin_mul"
, Mark ".L385"
, Mov "A"
    ( Variable "B" )
, Add "SP"
    ( Literal 2 )
, Mov "B"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Add "A"
    ( Variable "B" )
, Mov "C"
    ( Variable "A" )
, Load "A"
    ( Variable "SP" )
, Add "SP"
    ( Literal 1 )
, Mov "B"
    ( Variable "A" )
, Mov "A"
    ( Variable "C" )
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
, Mov "A"
    ( Variable ".L386" )
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
, Jmp "printf"
, Mark ".L386"
, Mov "A"
    ( Variable "B" )
, Add "SP"
    ( Literal 1 )
, Mark ".L381"
, Mark ".L116"
, PLoc 1 892 0
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
, PLoc 1 900 0
, Jmp ".L115"
, Mark ".L117"
, PLoc 1 903 0
, Mov "A"
    ( Literal 0 )
, Mov "B"
    ( Variable "A" )
, Exit
, Exit
]