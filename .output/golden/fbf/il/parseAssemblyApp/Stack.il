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
    ( Call "uneq"
        [ Variable "stop"
        , Literal 1
        ]
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
    ( Call "ifeq"
        [ Variable "temp"
        , Literal 10
        ]
    )
, Code
    ( Inc 1 "stop" )
, Code
    ( Call "end" [] )
, Code
    ( Call "ifeq"
        [ Variable "temp"
        , Literal 32
        ]
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
, Code
    ( Call "end" [] )
, Code
    ( Call "ifeq"
        [ Variable "found"
        , Literal 0
        ]
    )
, Code
    ( Push
        ( Variable "stack" ) "temp"
    )
, Code
    ( Call "end" [] )
, Code
    ( Call "end" [] )
]