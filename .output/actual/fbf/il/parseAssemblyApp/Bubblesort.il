[ Compiler
    ( LineBreaks 80 )
, Compiler
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
, Compiler
    ( Table 256 "mem" )
, Code Line
, Code
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
, Code Line
, Code
    ( Read "temp" )
, Code
    ( UnEq
        ( Literal 10 ) "temp" []
    )
, Code
    ( Inc 1 "tot" )
, Code
    ( WTable
        ( Variable "temp" )
        ( Variable "tot" ) "mem"
    )
, Code
    ( Read "temp" )
, Code End
, Code
    ( Msg
        [ "Wait"
        , "while"
        , "I"
        , "bubble-sort"
        , "the"
        , "characters..."
        ]
    )
, Code Line
, Code
    ( Inc 1 "cur" )
, Code
    ( Copy "tot" "lastchange" )
, Code
    ( UnEq
        ( Literal 0 ) "lastchange" []
    )
, Code
    ( Inc 1 "times" )
, Code Line
, Code
    ( Set 1 "subcur" )
, Code
    ( Inc 1 "tot" )
, Code
    ( UnEq
        ( Variable "tot" ) "subcur" []
    )
, Code
    ( RTable
        ( Variable "subcur" ) "mem" "temp"
    )
, Code
    ( Print [ "temp" ] )
, Code
    ( Inc 1 "subcur" )
, Code End
, Code
    ( Dec 1 "tot" )
, Code
    ( Set 1 "cur" )
, Code
    ( Copy "lastchange" "templastchange" )
, Code
    ( Set 0 "lastchange" )
, Code
    ( UnEq
        ( Variable "templastchange" ) "cur" []
    )
, Code
    ( RTable
        ( Variable "cur" ) "mem" "temp"
    )
, Code
    ( Inc 1 "cur" )
, Code
    ( RTable
        ( Variable "cur" ) "mem" "temp2"
    )
, Code
    ( Comp
        ( Variable "temp" )
        ( Variable "temp2" ) "compres"
    )
, Code
    ( IfEq
        ( Literal 0 ) "compres" []
    )
, Code
    ( Copy "cur" "lastchange" )
, Code
    ( WTable
        ( Variable "temp" )
        ( Variable "cur" ) "mem"
    )
, Code
    ( Dec 1 "cur" )
, Code
    ( WTable
        ( Variable "temp2" )
        ( Variable "cur" ) "mem"
    )
, Code
    ( Inc 1 "cur" )
, Code End
, Code End
, Code End
, Code
    ( Set 1 "cur" )
, Code
    ( Inc 1 "tot" )
, Code Line
, Code
    ( Msg
        [ "Finished!"
        , "It"
        , "took"
        , "me"
        ]
    )
, Code Space
, Code
    ( Byte2Ascii
        ( Variable "times" ) "temp" "temp2" "temp3"
    )
, Code
    ( Print
        [ "temp"
        , "temp2"
        , "temp3"
        ]
    )
, Code Space
, Code
    ( Msg
        [ "itterations."
        , "While"
        , "the"
        , "sentence"
        , "was"
        ]
    )
, Code Space
, Code
    ( Dec 1 "tot" )
, Code
    ( Byte2Ascii
        ( Variable "tot" ) "temp" "temp2" "temp3"
    )
, Code
    ( Print
        [ "temp"
        , "temp2"
        , "temp3"
        ]
    )
, Code Space
, Code
    ( Msg
        [ "characters"
        , "long."
        ]
    )
, Code Line
]