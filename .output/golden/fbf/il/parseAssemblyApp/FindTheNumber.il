[ Micro
    ( Compiler
        ( LineBreaks 80 )
    )
, Micro
    ( Compiler
        ( Dim
            [ "low"
            , "high"
            , "guess"
            , "temp"
            , "temp2"
            , "temp3"
            ]
        )
    )
, Micro
    ( Code
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
    )
, Micro
    ( Code
        ( Set 1 "low" )
    )
, Micro
    ( Code
        ( Set 100 "high" )
    )
, Micro
    ( Code
        ( Read "temp" )
    )
, Micro
    ( Code
        ( UnEq
            ( Variable "high" ) "low" []
        )
    )
, Micro
    ( Code
        ( Sub
            ( Variable "high" )
            ( Variable "low" ) "guess"
        )
    )
, Micro
    ( Code
        ( Div
            ( Variable "guess" )
            ( Literal 2 ) "guess"
        )
    )
, Micro
    ( Code
        ( Add
            ( Variable "low" )
            ( Variable "guess" ) "guess"
        )
    )
, Micro
    ( Code
        ( Byte2Ascii
            ( Variable "guess" ) "temp" "temp2" "temp3"
        )
    )
, Micro
    ( Code
        ( Msg
            [ "My"
            , "guess"
            , "is"
            ]
        )
    )
, Micro ( Code Space )
, Micro
    ( Code
        ( Print
            [ "temp"
            , "temp2"
            , "temp3"
            ]
        )
    )
, Micro
    ( Code
        ( Msg [ "." ] )
    )
, Micro ( Code Line )
, Micro
    ( Code
        ( Msg
            [ "1."
            , "Correct!"
            ]
        )
    )
, Micro ( Code Line )
, Micro
    ( Code
        ( Msg
            [ "2."
            , "Smaller"
            , "than"
            , "the"
            , "hidden"
            , "number."
            ]
        )
    )
, Micro ( Code Line )
, Micro
    ( Code
        ( Msg
            [ "3."
            , "Bigger"
            , "than"
            , "the"
            , "hidden"
            , "number."
            ]
        )
    )
, Micro ( Code Line )
, Micro
    ( Code
        ( Read "temp" )
    )
, Micro
    ( Code
        ( Read "temp2" )
    )
, Micro
    ( Code
        ( IfEq
            ( Literal 49 ) "temp" []
        )
    )
, Micro
    ( Code
        ( Copy "guess" "low" )
    )
, Micro
    ( Code
        ( Copy "low" "high" )
    )
, Micro ( Code End )
, Micro
    ( Code
        ( IfEq
            ( Literal 50 ) "temp" []
        )
    )
, Micro
    ( Code
        ( Copy "guess" "high" )
    )
, Micro
    ( Code
        ( Dec 1 "high" )
    )
, Micro ( Code End )
, Micro
    ( Code
        ( IfEq
            ( Literal 51 ) "temp" []
        )
    )
, Micro
    ( Code
        ( Copy "guess" "low" )
    )
, Micro
    ( Code
        ( Inc 1 "low" )
    )
, Micro ( Code End )
, Micro
    ( Code
        ( Comp
            ( Variable "low" )
            ( Variable "high" ) "temp"
        )
    )
, Micro
    ( Code
        ( IfEq
            ( Literal 1 ) "temp" []
        )
    )
, Micro
    ( Code
        ( Msg
            [ "I"
            , "found"
            , "it!"
            , "It's"
            ]
        )
    )
, Micro ( Code Space )
, Micro
    ( Code
        ( Byte2Ascii
            ( Variable "low" ) "temp" "temp2" "temp3"
        )
    )
, Micro
    ( Code
        ( Print
            [ "temp"
            , "temp2"
            , "temp3"
            ]
        )
    )
, Micro
    ( Code
        ( Msg [ "." ] )
    )
, Micro ( Code Line )
, Micro ( Code End )
, Micro ( Code Line )
, Micro ( Code End )
]