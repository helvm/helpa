[ Micro
    ( Compiler
        ( LineBreaks 80 )
    )
, Micro
    ( Compiler
        ( Dim
            [ "tot"
            , "cur"
            , "subcur"
            , "temp"
            , "lastchange"
            , "templastchange"
            , "temp2"
            , "compres"
            , "times"
            , "temp3"
            ]
        )
    )
, Micro
    ( Compiler
        ( Table 256 "mem" )
    )
, Micro ( Code Line )
, Micro
    ( Code
        ( Msg
            [ "OK"
            , "give"
            , "me"
            , "a"
            , "sentence"
            , "and"
            , "then"
            , "press"
            , "enter."
            , "I"
            , "cannot"
            , "hold"
            , "more"
            , "than"
            , "256"
            , "letters"
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
        ( UnEq
            ( Literal 10 ) "temp" []
        )
    )
, Micro
    ( Code
        ( Inc 1 "tot" )
    )
, Micro
    ( Code
        ( WTable
            ( Variable "temp" )
            ( Variable "tot" ) "mem"
        )
    )
, Micro
    ( Code
        ( Read "temp" )
    )
, Micro ( Code End )
, Micro
    ( Code
        ( Msg
            [ "Wait"
            , "while"
            , "I"
            , "bubble-sort"
            , "the"
            , "characters..."
            ]
        )
    )
, Micro ( Code Line )
, Micro
    ( Code
        ( Inc 1 "cur" )
    )
, Micro
    ( Code
        ( Copy "tot" "lastchange" )
    )
, Micro
    ( Code
        ( UnEq
            ( Literal 0 ) "lastchange" []
        )
    )
, Micro
    ( Code
        ( Inc 1 "times" )
    )
, Micro ( Code Line )
, Micro
    ( Code
        ( Set 1 "subcur" )
    )
, Micro
    ( Code
        ( Inc 1 "tot" )
    )
, Micro
    ( Code
        ( UnEq
            ( Variable "tot" ) "subcur" []
        )
    )
, Micro
    ( Code
        ( RTable
            ( Variable "subcur" ) "mem" "temp"
        )
    )
, Micro
    ( Code
        ( Print [ "temp" ] )
    )
, Micro
    ( Code
        ( Inc 1 "subcur" )
    )
, Micro ( Code End )
, Micro
    ( Code
        ( Dec 1 "tot" )
    )
, Micro
    ( Code
        ( Set 1 "cur" )
    )
, Micro
    ( Code
        ( Copy "lastchange" "templastchange" )
    )
, Micro
    ( Code
        ( Set 0 "lastchange" )
    )
, Micro
    ( Code
        ( UnEq
            ( Variable "templastchange" ) "cur" []
        )
    )
, Micro
    ( Code
        ( RTable
            ( Variable "cur" ) "mem" "temp"
        )
    )
, Micro
    ( Code
        ( Inc 1 "cur" )
    )
, Micro
    ( Code
        ( RTable
            ( Variable "cur" ) "mem" "temp2"
        )
    )
, Micro
    ( Code
        ( Comp
            ( Variable "temp" )
            ( Variable "temp2" ) "compres"
        )
    )
, Micro
    ( Code
        ( IfEq
            ( Literal 0 ) "compres" []
        )
    )
, Micro
    ( Code
        ( Copy "cur" "lastchange" )
    )
, Micro
    ( Code
        ( WTable
            ( Variable "temp" )
            ( Variable "cur" ) "mem"
        )
    )
, Micro
    ( Code
        ( Dec 1 "cur" )
    )
, Micro
    ( Code
        ( WTable
            ( Variable "temp2" )
            ( Variable "cur" ) "mem"
        )
    )
, Micro
    ( Code
        ( Inc 1 "cur" )
    )
, Micro ( Code End )
, Micro ( Code End )
, Micro ( Code End )
, Micro
    ( Code
        ( Set 1 "cur" )
    )
, Micro
    ( Code
        ( Inc 1 "tot" )
    )
, Micro ( Code Line )
, Micro
    ( Code
        ( Msg
            [ "Finished!"
            , "It"
            , "took"
            , "me"
            ]
        )
    )
, Micro ( Code Space )
, Micro
    ( Code
        ( Byte2Ascii
            ( Variable "times" ) "temp" "temp2" "temp3"
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
, Micro ( Code Space )
, Micro
    ( Code
        ( Msg
            [ "itterations."
            , "While"
            , "the"
            , "sentence"
            , "was"
            ]
        )
    )
, Micro ( Code Space )
, Micro
    ( Code
        ( Dec 1 "tot" )
    )
, Micro
    ( Code
        ( Byte2Ascii
            ( Variable "tot" ) "temp" "temp2" "temp3"
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
, Micro ( Code Space )
, Micro
    ( Code
        ( Msg
            [ "characters"
            , "long."
            ]
        )
    )
, Micro ( Code Line )
]