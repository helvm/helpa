[ Micro
    ( Compiler
        ( Dim
            [ "exit"
            , "char"
            , "seed"
            , "temp"
            ]
        )
    )
, Micro
    ( Compiler
        ( LineBreaks 80 )
    )
, Micro
    ( Code
        ( Msg
            [ "Welcome"
            , "to"
            , "Brainfuck"
            , "Lightass!"
            ]
        )
    )
, Micro ( Code Line )
, Micro
    ( Code
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
    )
, Micro ( Code Line )
, Micro ( Code Line )
, Micro
    ( Code
        ( Msg
            [ "Give"
            , "the"
            , "plot"
            , "character:"
            ]
        )
    )
, Micro
    ( Code
        ( Read "seed" )
    )
, Micro ( Code Line )
, Micro
    ( Code
        ( Msg
            [ "Give"
            , "me"
            , "question."
            ]
        )
    )
, Micro ( Code Line )
, Micro
    ( Code
        ( Copy "seed" "temp" )
    )
, Micro
    ( Code
        ( UnEq
            ( Literal 1 ) "exit" []
        )
    )
, Micro
    ( Code
        ( Read "char" )
    )
, Micro
    ( Code
        ( Add
            ( Variable "temp" )
            ( Variable "char" ) "temp"
        )
    )
, Micro
    ( Code
        ( IfEq
            ( Literal 10 ) "char" []
        )
    )
, Micro
    ( Code
        ( Mod
            ( Variable "temp" )
            ( Literal 10 ) "temp"
        )
    )
, Micro
    ( Code
        ( Comp
            ( Variable "temp" )
            ( Literal 3 ) "temp"
        )
    )
, Micro
    ( Code
        ( IfEq
            ( Literal 2 ) "temp" []
        )
    )
, Micro
    ( Code
        ( Msg [ "Yes." ] )
    )
, Micro ( Code End )
, Micro
    ( Code
        ( IfNotEq
            ( Literal 2 ) "temp" []
        )
    )
, Micro
    ( Code
        ( Msg [ "No." ] )
    )
, Micro ( Code End )
, Micro ( Code Line )
, Micro
    ( Code
        ( Copy "seed" "temp" )
    )
, Micro
    ( Code
        ( Msg
            [ "Give"
            , "me"
            , "question."
            ]
        )
    )
, Micro ( Code Line )
, Micro ( Code End )
, Micro ( Code End )
]