[ Micro
    ( Compiler
        ( Dim
            [ "i"
            , "j"
            , "temp"
            , "run"
            ]
        )
    )
, Micro
    ( Compiler
        ( Dim
            [ "random"
            , "curplace"
            ]
        )
    )
, Micro
    ( Compiler
        ( Dim
            [ "fliptemp"
            , "fliptemp2"
            ]
        )
    )
, Micro
    ( Compiler
        ( LineBreaks 80 )
    )
, Micro ( Compiler ByteCells )
, Micro
    ( Compiler
        ( Table 256 "pairs" )
    )
, Micro
    ( Compiler
        ( Table 256 "totals" )
    )
, Def [] "createtable" []
, Micro
    ( Code
        ( Set 255 "i" )
    )
, Micro
    ( Code
        ( UnEq
            ( Literal 0 ) "i" []
        )
    )
, Micro
    ( Code
        ( WTable
            ( Variable "i" )
            ( Variable "i" ) "pairs"
        )
    )
, Micro
    ( Code
        ( Dec 1 "i" )
    )
, Micro ( Code End )
, Micro
    ( Code
        ( WTable
            ( Variable "i" )
            ( Variable "i" ) "pairs"
        )
    )
, Micro ( Compiler EndBlock )
, Def [] "advancerandom" []
, Micro
    ( Code
        ( Multi
            ( Variable "random" )
            ( Variable "random" ) "temp"
        )
    )
, Micro
    ( Code
        ( Inc 7 "random" )
    )
, Micro
    ( Code
        ( Multi
            ( Variable "temp" )
            ( Variable "random" ) "temp"
        )
    )
, Micro
    ( Code
        ( Inc 1 "temp" )
    )
, Micro
    ( Code
        ( Multi
            ( Variable "random" )
            ( Variable "temp" ) "random"
        )
    )
, Micro
    ( Code
        ( Add
            ( Variable "random" )
            ( Variable "curplace" ) "random"
        )
    )
, Micro
    ( Code
        ( RTable
            ( Variable "random" ) "pairs" "random"
        )
    )
, Micro ( Compiler EndBlock )
, Def
    [ "flip_initial"
    , "flip_final"
    ] "flip" []
, Micro
    ( Code
        ( RTable
            ( Variable "flip_initial" ) "pairs" "fliptemp"
        )
    )
, Micro
    ( Code
        ( RTable
            ( Variable "flip_final" ) "pairs" "fliptemp2"
        )
    )
, Micro
    ( Code
        ( WTable
            ( Variable "fliptemp2" )
            ( Variable "flip_initial" ) "pairs"
        )
    )
, Micro
    ( Code
        ( WTable
            ( Variable "fliptemp" )
            ( Variable "flip_final" ) "pairs"
        )
    )
, Micro ( Compiler EndBlock )
, Call [] "createtable"
, Micro
    ( Code
        ( Read "random" )
    )
, Micro
    ( Code
        ( Set 1 "run" )
    )
, Micro
    ( Code
        ( UnEq
            ( Literal 0 ) "run" []
        )
    )
, Micro
    ( Code
        ( Add
            ( Variable "temp" )
            ( Variable "random" ) "temp"
        )
    )
, Call [] "advancerandom"
, Call
    [ Variable "random"
    , Variable "curplace"
    ] "flip"
, Micro
    ( Code
        ( Inc 1 "curplace" )
    )
, Call [] "advancerandom"
, Micro
    ( Code
        ( Add
            ( Variable "temp" )
            ( Variable "random" ) "temp"
        )
    )
, Micro
    ( Code
        ( RTable
            ( Variable "random" ) "totals" "temp"
        )
    )
, Micro
    ( Code
        ( Inc 1 "temp" )
    )
, Micro
    ( Code
        ( WTable
            ( Variable "temp" )
            ( Variable "random" ) "totals"
        )
    )
, Micro
    ( Code
        ( IfEq
            ( Literal 100 ) "temp" []
        )
    )
, Micro
    ( Code
        ( Set 0 "run" )
    )
, Micro ( Code End )
, Micro
    ( Code
        ( Msg [ "." ] )
    )
, Micro ( Code End )
, Micro ( Code Line )
, Micro
    ( Code
        ( Set 0 "run" )
    )
, Micro
    ( Code
        ( Byte2Ascii
            ( Variable "run" ) "i" "j" "temp"
        )
    )
, Micro
    ( Code
        ( Print
            [ "i"
            , "j"
            , "temp"
            ]
        )
    )
, Micro ( Code Space )
, Micro
    ( Code
        ( RTable
            ( Variable "i" ) "totals" "temp"
        )
    )
, Micro
    ( Code
        ( Byte2Ascii
            ( Variable "temp" ) "i" "j" "temp"
        )
    )
, Micro
    ( Code
        ( Print
            [ "i"
            , "j"
            , "temp"
            ]
        )
    )
, Micro ( Code Line )
, Micro
    ( Code
        ( UnEq
            ( Literal 255 ) "run" []
        )
    )
, Micro
    ( Code
        ( Byte2Ascii
            ( Variable "run" ) "i" "j" "temp"
        )
    )
, Micro
    ( Code
        ( Print
            [ "i"
            , "j"
            , "temp"
            ]
        )
    )
, Micro ( Code Space )
, Micro
    ( Code
        ( RTable
            ( Variable "run" ) "totals" "temp"
        )
    )
, Micro
    ( Code
        ( Byte2Ascii
            ( Variable "temp" ) "i" "j" "temp"
        )
    )
, Micro
    ( Code
        ( Print
            [ "i"
            , "j"
            , "temp"
            ]
        )
    )
, Micro ( Code Line )
, Micro
    ( Code
        ( Inc 1 "run" )
    )
, Micro ( Code End )
]