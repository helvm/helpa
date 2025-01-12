[ Compiler
    ( Dim
        [ "i"
        , "j"
        , "temp"
        , "run"
        ]
    )
, Compiler
    ( Dim
        [ "random"
        , "curplace"
        ]
    )
, Compiler
    ( Dim
        [ "fliptemp"
        , "fliptemp2"
        ]
    )
, Compiler
    ( LineBreaks 80 )
, Compiler ByteCells
, Compiler
    ( Table 256 "pairs" )
, Compiler
    ( Table 256 "totals" )
]