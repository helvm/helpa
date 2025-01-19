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
    ( Call "uneq"
        [ Variable "exit"
        , Literal 1
        ]
    )
, Code
    ( Read "char" )
, Code
    ( Add
        ( Variable "temp" )
        ( Variable "char" ) "temp"
    )
, Code
    ( Call "ifeq"
        [ Variable "char"
        , Literal 10
        ]
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
    ( Call "ifeq"
        [ Variable "temp"
        , Literal 2
        ]
    )
, Code
    ( Msg [ "Yes." ] )
, Code
    ( Call "end" [] )
, Code
    ( Call "ifnoteq"
        [ Variable "temp"
        , Literal 2
        ]
    )
, Code
    ( Msg [ "No." ] )
, Code
    ( Call "end" [] )
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
, Code
    ( Call "end" [] )
, Code
    ( Call "end" [] )
]