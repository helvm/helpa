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
    ( Block "createtable" []
        [ Code
            ( Set 255 "i" )
        , Code
            ( Call "uneq"
                [ Variable "i"
                , Literal 0
                ]
            )
        , Code
            ( WTable
                ( Variable "i" )
                ( Variable "i" ) "pairs"
            )
        , Code
            ( Dec 1 "i" )
        , Code
            ( Call "end" [] )
        , Code
            ( WTable
                ( Variable "i" )
                ( Variable "i" ) "pairs"
            )
        ]
    )
, Compiler
    ( Block "advancerandom" []
        [ Code
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
        ]
    )
, Compiler
    ( Block "flip"
        [ "flip_initial"
        , "flip_final"
        ]
        [ Code
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
        ]
    )
, Code
    ( Call "createtable" [] )
, Code
    ( Read "random" )
, Code
    ( Set 1 "run" )
, Code
    ( Call "uneq"
        [ Variable "run"
        , Literal 0
        ]
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
    ( Call "ifeq"
        [ Variable "temp"
        , Literal 100
        ]
    )
, Code
    ( Set 0 "run" )
, Code
    ( Call "end" [] )
, Code
    ( Msg [ "." ] )
, Code
    ( Call "end" [] )
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
    ( Call "uneq"
        [ Variable "run"
        , Literal 255
        ]
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
, Code
    ( Call "end" [] )
]