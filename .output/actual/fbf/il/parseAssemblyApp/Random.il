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
, Compiler
    ( Block "createtable" [] [] )
, Code
    ( Set 255 "i" )
, Code
    ( UnEq
        ( Literal 0 ) "i" []
    )
, Code
    ( WTable
        ( Variable "i" )
        ( Variable "i" ) "pairs"
    )
, Code
    ( Dec 1 "i" )
, Code End
, Code
    ( WTable
        ( Variable "i" )
        ( Variable "i" ) "pairs"
    )
, Compiler EndBlock
, Compiler
    ( Block "advancerandom" [] [] )
, Code
    ( Multi
        ( Variable "random" )
        ( Variable "random" ) "temp"
    )
, Code
    ( Inc 7 "random" )
, Code
    ( Multi
        ( Variable "temp" )
        ( Variable "random" ) "temp"
    )
, Code
    ( Inc 1 "temp" )
, Code
    ( Multi
        ( Variable "random" )
        ( Variable "temp" ) "random"
    )
, Code
    ( Add
        ( Variable "random" )
        ( Variable "curplace" ) "random"
    )
, Code
    ( RTable
        ( Variable "random" ) "pairs" "random"
    )
, Compiler EndBlock
, Compiler
    ( Block "flip"
        [ "flip_initial"
        , "flip_final"
        ] []
    )
, Code
    ( RTable
        ( Variable "flip_initial" ) "pairs" "fliptemp"
    )
, Code
    ( RTable
        ( Variable "flip_final" ) "pairs" "fliptemp2"
    )
, Code
    ( WTable
        ( Variable "fliptemp2" )
        ( Variable "flip_initial" ) "pairs"
    )
, Code
    ( WTable
        ( Variable "fliptemp" )
        ( Variable "flip_final" ) "pairs"
    )
, Compiler EndBlock
, Code
    ( Call "createtable" [] )
, Code
    ( Read "random" )
, Code
    ( Set 1 "run" )
, Code
    ( UnEq
        ( Literal 0 ) "run" []
    )
, Code
    ( Add
        ( Variable "temp" )
        ( Variable "random" ) "temp"
    )
, Code
    ( Call "advancerandom" [] )
, Code
    ( Call "flip"
        [ Variable "random"
        , Variable "curplace"
        ]
    )
, Code
    ( Inc 1 "curplace" )
, Code
    ( Call "advancerandom" [] )
, Code
    ( Add
        ( Variable "temp" )
        ( Variable "random" ) "temp"
    )
, Code
    ( RTable
        ( Variable "random" ) "totals" "temp"
    )
, Code
    ( Inc 1 "temp" )
, Code
    ( WTable
        ( Variable "temp" )
        ( Variable "random" ) "totals"
    )
, Code
    ( IfEq
        ( Literal 100 ) "temp" []
    )
, Code
    ( Set 0 "run" )
, Code End
, Code
    ( Msg [ "." ] )
, Code End
, Code Line
, Code
    ( Set 0 "run" )
, Code
    ( Byte2Ascii
        ( Variable "run" ) "i" "j" "temp"
    )
, Code
    ( Print
        [ "i"
        , "j"
        , "temp"
        ]
    )
, Code Space
, Code
    ( RTable
        ( Variable "i" ) "totals" "temp"
    )
, Code
    ( Byte2Ascii
        ( Variable "temp" ) "i" "j" "temp"
    )
, Code
    ( Print
        [ "i"
        , "j"
        , "temp"
        ]
    )
, Code Line
, Code
    ( UnEq
        ( Literal 255 ) "run" []
    )
, Code
    ( Byte2Ascii
        ( Variable "run" ) "i" "j" "temp"
    )
, Code
    ( Print
        [ "i"
        , "j"
        , "temp"
        ]
    )
, Code Space
, Code
    ( RTable
        ( Variable "run" ) "totals" "temp"
    )
, Code
    ( Byte2Ascii
        ( Variable "temp" ) "i" "j" "temp"
    )
, Code
    ( Print
        [ "i"
        , "j"
        , "temp"
        ]
    )
, Code Line
, Code
    ( Inc 1 "run" )
, Code End
]