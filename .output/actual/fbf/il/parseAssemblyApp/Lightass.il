[ Compiler
    ( Dim
        [ "exit"
        , "char"
        , "seed"
        , "temp"
        ]
    )
, Compiler
    ( LineBreaks 80 )
, Code
    ( Msg
        [ "Welcome"
        , "to"
        , "Brainfuck"
        , "Lightass!"
        ]
    )
, Code Line
, Code
    ( Msg
        [ "To"
        , "download"
        , "the"
        , "original"
        , "Lightass"
        , "visit:"
        , "http://students.ceid.upatras.gr"
        ]
    )
, Code Line
, Code Line
, Code
    ( Msg
        [ "Give"
        , "the"
        , "plot"
        , "character:"
        ]
    )
, Code
    ( Read "seed" )
, Code Line
, Code
    ( Msg
        [ "Give"
        , "me"
        , "question."
        ]
    )
, Code Line
, Code
    ( Copy "seed" "temp" )
, Code
    ( UnEq
        ( Literal 1 ) "exit" []
    )
, Code
    ( Read "char" )
, Code
    ( Add
        ( Variable "temp" )
        ( Variable "char" ) "temp"
    )
, Code
    ( IfEq
        ( Literal 10 ) "char" []
    )
, Code
    ( Mod
        ( Variable "temp" )
        ( Literal 10 ) "temp"
    )
, Code
    ( Comp
        ( Variable "temp" )
        ( Literal 3 ) "temp"
    )
, Code
    ( IfEq
        ( Literal 2 ) "temp" []
    )
, Code
    ( Msg [ "Yes." ] )
, Code End
, Code
    ( IfNotEq
        ( Literal 2 ) "temp" []
    )
, Code
    ( Msg [ "No." ] )
, Code End
, Code Line
, Code
    ( Copy "seed" "temp" )
, Code
    ( Msg
        [ "Give"
        , "me"
        , "question."
        ]
    )
, Code Line
, Code End
, Code End
]