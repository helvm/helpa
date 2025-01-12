[ Compiler
    ( LineBreaks 80 )
, Compiler
    ( Dim
        [ "stop"
        , "temp"
        , "found"
        , "a"
        , "b"
        , "c"
        ]
    )
, Compiler
    ( Table 256 "stack" )
, Code
    ( UnEq
        ( Literal 1 ) "stop" []
    )
, Code
    ( CopySize "stack" "a" )
, Code
    ( Byte2Ascii
        ( Variable "a" ) "a" "b" "c"
    )
, Code
    ( Print
        [ "a"
        , "b"
        , "c"
        ]
    )
, Code
    ( Read "temp" )
, Code
    ( Msg
        [ "clear"
        , "3"
        ]
    )
, Code
    ( Set 0 "found" )
, Code
    ( IfEq
        ( Literal 10 ) "temp" []
    )
, Code
    ( Inc 1 "stop" )
, Code End
, Code
    ( IfEq
        ( Literal 32 ) "temp" []
    )
, Code
    ( Inc 1 "found" )
, Code
    ( Pop "stack" "temp" )
, Code
    ( Print [ "temp" ] )
, Code
    ( Read "temp" )
, Code
    ( Msg
        [ "clear"
        , "1"
        ]
    )
, Code End
, Code
    ( IfEq
        ( Literal 0 ) "found" []
    )
, Code
    ( Push
        ( Variable "stack" ) "temp"
    )
, Code End
, Code End
]