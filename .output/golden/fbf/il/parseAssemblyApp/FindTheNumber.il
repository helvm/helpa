[ Compiler
    ( LineBreaks 80 )
, Compiler
    ( Dim
        [ "low"
        , "high"
        , "guess"
        , "temp"
        , "temp2"
        , "temp3"
        ]
    )
, Code
    ( Msg
        [ "Think"
        , "of"
        , "a"
        , "number"
        , "between"
        , "1"
        , "and"
        , "100."
        , "And"
        , "press"
        , "any"
        , "key"
        , "when"
        , "ready."
        ]
    )
, Code
    ( Set 1 "low" )
, Code
    ( Set 100 "high" )
, Code
    ( Read "temp" )
, Code
    ( Call "uneq"
        [ Variable "low"
        , Variable "high"
        ]
    )
, Code
    ( Sub
        ( Variable "high" )
        ( Variable "low" ) "guess"
    )
, Code
    ( Div
        ( Variable "guess" )
        ( Literal 2 ) "guess"
    )
, Code
    ( Add
        ( Variable "low" )
        ( Variable "guess" ) "guess"
    )
, Code
    ( Byte2Ascii
        ( Variable "guess" ) "temp" "temp2" "temp3"
    )
, Code
    ( Msg
        [ "My"
        , "guess"
        , "is"
        ]
    )
, Code Space
, Code
    ( Print
        [ "temp"
        , "temp2"
        , "temp3"
        ]
    )
, Code
    ( Msg [ "." ] )
, Code Line
, Code
    ( Msg
        [ "1."
        , "Correct!"
        ]
    )
, Code Line
, Code
    ( Msg
        [ "2."
        , "Smaller"
        , "than"
        , "the"
        , "hidden"
        , "number."
        ]
    )
, Code Line
, Code
    ( Msg
        [ "3."
        , "Bigger"
        , "than"
        , "the"
        , "hidden"
        , "number."
        ]
    )
, Code Line
, Code
    ( Read "temp" )
, Code
    ( Read "temp2" )
, Code
    ( Call "ifeq"
        [ Variable "temp"
        , Literal 49
        ]
    )
, Code
    ( Copy "guess" "low" )
, Code
    ( Copy "low" "high" )
, Code
    ( Call "end" [] )
, Code
    ( Call "ifeq"
        [ Variable "temp"
        , Literal 50
        ]
    )
, Code
    ( Copy "guess" "high" )
, Code
    ( Dec 1 "high" )
, Code
    ( Call "end" [] )
, Code
    ( Call "ifeq"
        [ Variable "temp"
        , Literal 51
        ]
    )
, Code
    ( Copy "guess" "low" )
, Code
    ( Inc 1 "low" )
, Code
    ( Call "end" [] )
, Code
    ( Comp
        ( Variable "low" )
        ( Variable "high" ) "temp"
    )
, Code
    ( Call "ifeq"
        [ Variable "temp"
        , Literal 1
        ]
    )
, Code
    ( Msg
        [ "I"
        , "found"
        , "it!"
        , "It's"
        ]
    )
, Code Space
, Code
    ( Byte2Ascii
        ( Variable "low" ) "temp" "temp2" "temp3"
    )
, Code
    ( Print
        [ "temp"
        , "temp2"
        , "temp3"
        ]
    )
, Code
    ( Msg [ "." ] )
, Code Line
, Code
    ( Call "end" [] )
, Code Line
, Code
    ( Call "end" [] )
]