[ Micro
    ( Compiler
        ( LineBreaks 80 )
    )
, Micro
    ( Compiler
        ( Dim
            [ "stop"
            , "temp"
            , "found"
            , "a"
            , "b"
            , "c"
            ]
        )
    )
, Micro
    ( Compiler
        ( Table 256 "stack" )
    )
, Micro
    ( Code
        ( UnEq
            ( Literal 1 ) "stop" []
        )
    )
, Micro
    ( Code
        ( CopySize "stack" "a" )
    )
, Micro
    ( Code
        ( Byte2Ascii
            ( Variable "a" ) "a" "b" "c"
        )
    )
, Micro
    ( Code
        ( Print
            [ "a"
            , "b"
            , "c"
            ]
        )
    )
, Micro
    ( Code
        ( Read "temp" )
    )
, Micro
    ( Code
        ( Msg
            [ "clear"
            , "3"
            ]
        )
    )
, Micro
    ( Code
        ( Set 0 "found" )
    )
, Micro
    ( Code
        ( IfEq
            ( Literal 10 ) "temp" []
        )
    )
, Micro
    ( Code
        ( Inc 1 "stop" )
    )
, Micro ( Code End )
, Micro
    ( Code
        ( IfEq
            ( Literal 32 ) "temp" []
        )
    )
, Micro
    ( Code
        ( Inc 1 "found" )
    )
, Micro
    ( Code
        ( Pop "stack" "temp" )
    )
, Micro
    ( Code
        ( Print [ "temp" ] )
    )
, Micro
    ( Code
        ( Read "temp" )
    )
, Micro
    ( Code
        ( Msg
            [ "clear"
            , "1"
            ]
        )
    )
, Micro ( Code End )
, Micro
    ( Code
        ( IfEq
            ( Literal 0 ) "found" []
        )
    )
, Micro
    ( Code
        ( Push
            ( Variable "stack" ) "temp"
        )
    )
, Micro ( Code End )
, Micro ( Code End )
]