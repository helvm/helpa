[ Micro
    ( Compiler
        ( LineBreaks 80 )
    )
, Micro
    ( Compiler
        ( Dim [ "stones" ] )
    )
, Micro
    ( Compiler
        ( Dim [ "humanturn" ] )
    )
, Micro
    ( Compiler
        ( Dim [ "a" ] )
    )
, Micro
    ( Compiler
        ( Dim [ "b" ] )
    )
, Micro
    ( Compiler
        ( Dim [ "c" ] )
    )
, Micro
    ( Compiler
        ( Dim [ "max" ] )
    )
, Micro
    ( Compiler
        ( Dim [ "maxplusone" ] )
    )
, Micro
    ( Compiler
        ( Dim [ "drawstone" ] )
    )
, Micro ( Code Line )
, Micro
    ( Code
        ( Msg
            [ "How"
            , "many"
            , "stones"
            , "will"
            , "we"
            , "have"
            , "initially?"
            , "Write"
            , "a"
            , "number"
            , "smaller"
            , "than"
            , "256"
            , "and"
            , "then"
            , "press"
            , "enter."
            ]
        )
    )
, Micro ( Code Line )
, Micro
    ( Code
        ( Read "stones" )
    )
, Micro
    ( Code
        ( Dec 48 "stones" )
    )
, Micro
    ( Code
        ( Read "a" )
    )
, Micro
    ( Code
        ( IfNotEq
            ( Literal 10 ) "a" []
        )
    )
, Micro
    ( Code
        ( Multi
            ( Variable "stones" )
            ( Literal 10 ) "stones"
        )
    )
, Micro
    ( Code
        ( Dec 48 "a" )
    )
, Micro
    ( Code
        ( Add
            ( Variable "stones" )
            ( Variable "a" ) "stones"
        )
    )
, Micro
    ( Code
        ( Read "a" )
    )
, Micro
    ( Code
        ( IfNotEq
            ( Literal 10 ) "a" []
        )
    )
, Micro
    ( Code
        ( Multi
            ( Variable "stones" )
            ( Literal 10 ) "stones"
        )
    )
, Micro
    ( Code
        ( Dec 48 "a" )
    )
, Micro
    ( Code
        ( Add
            ( Variable "stones" )
            ( Variable "a" ) "stones"
        )
    )
, Micro
    ( Code
        ( Read "a" )
    )
, Micro ( Code End )
, Micro ( Code End )
, Micro ( Code Line )
, Micro
    ( Code
        ( Msg
            [ "How"
            , "many"
            , "stones"
            , "will"
            , "we"
            , "be"
            , "allowed"
            , "to"
            , "draw"
            , "per"
            , "turn?"
            , "Write"
            , "a"
            , "number"
            , "between"
            , "1"
            , "and"
            , "9."
            ]
        )
    )
, Micro ( Code Line )
, Micro
    ( Code
        ( Read "max" )
    )
, Micro
    ( Code
        ( Dec 48 "max" )
    )
, Micro
    ( Code
        ( Add
            ( Variable "max" )
            ( Literal 1 ) "maxplusone"
        )
    )
, Micro ( Code Line )
, Micro ( Code Line )
, Micro
    ( Code
        ( Msg
            [ "Shall"
            , "I"
            , "go"
            , "first?"
            , "(y/n)"
            ]
        )
    )
, Micro ( Code Line )
, Micro
    ( Code
        ( Read "a" )
    )
, Micro
    ( Code
        ( IfEq
            ( Literal 110 ) "a" []
        )
    )
, Micro
    ( Code
        ( Set 1 "humanturn" )
    )
, Micro ( Code End )
, Micro ( Code Line )
, Micro ( Code Line )
, Micro
    ( Code
        ( UnEq
            ( Literal 0 ) "stones" []
        )
    )
, Micro
    ( Code
        ( Msg [ "Stones:" ] )
    )
, Micro ( Code Space )
, Micro
    ( Code
        ( Copy "stones" "drawstone" )
    )
, Micro
    ( Code
        ( UnEq
            ( Literal 0 ) "drawstone" []
        )
    )
, Micro
    ( Code
        ( Msg [ "O" ] )
    )
, Micro
    ( Code
        ( Dec 1 "drawstone" )
    )
, Micro ( Code End )
, Micro ( Code Space )
, Micro
    ( Code
        ( Msg [ "=" ] )
    )
, Micro ( Code Space )
, Micro
    ( Code
        ( Div
            ( Variable "stones" )
            ( Literal 100 ) "a"
        )
    )
, Micro
    ( Code
        ( Mod
            ( Variable "stones" )
            ( Literal 100 ) "b"
        )
    )
, Micro
    ( Code
        ( Mod
            ( Variable "b" )
            ( Literal 10 ) "c"
        )
    )
, Micro
    ( Code
        ( Div
            ( Variable "b" )
            ( Literal 10 ) "b"
        )
    )
, Micro
    ( Code
        ( Inc 48 "a" )
    )
, Micro
    ( Code
        ( Inc 48 "b" )
    )
, Micro
    ( Code
        ( Inc 48 "c" )
    )
, Micro
    ( Code
        ( IfNotEq
            ( Literal 48 ) "a" []
        )
    )
, Micro
    ( Code
        ( Print [ "a" ] )
    )
, Micro ( Code End )
, Micro
    ( Code
        ( IfEq
            ( Literal 48 ) "b" []
        )
    )
, Micro
    ( Code
        ( IfNotEq
            ( Literal 48 ) "a" []
        )
    )
, Micro
    ( Code
        ( Print [ "b" ] )
    )
, Micro ( Code End )
, Micro ( Code End )
, Micro
    ( Code
        ( IfNotEq
            ( Literal 48 ) "b" []
        )
    )
, Micro
    ( Code
        ( Print [ "b" ] )
    )
, Micro ( Code End )
, Micro
    ( Code
        ( Print [ "c" ] )
    )
, Micro ( Code Line )
, Micro
    ( Code
        ( IfEq
            ( Literal 0 ) "humanturn" []
        )
    )
, Micro
    ( Code
        ( Mod
            ( Variable "stones" )
            ( Variable "maxplusone" ) "a"
        )
    )
, Micro
    ( Code
        ( IfEq
            ( Literal 0 ) "a" []
        )
    )
, Micro
    ( Code
        ( Set 1 "a" )
    )
, Micro ( Code End )
, Micro
    ( Code
        ( Sub
            ( Variable "stones" )
            ( Variable "a" ) "stones"
        )
    )
, Micro
    ( Code
        ( Msg
            [ "I"
            , "will"
            , "draw"
            ]
        )
    )
, Micro ( Code Space )
, Micro
    ( Code
        ( Inc 48 "a" )
    )
, Micro
    ( Code
        ( Print [ "a" ] )
    )
, Micro ( Code Space )
, Micro
    ( Code
        ( Dec 48 "a" )
    )
, Micro
    ( Code
        ( IfEq
            ( Literal 1 ) "a" []
        )
    )
, Micro
    ( Code
        ( Msg [ "stone." ] )
    )
, Micro ( Code End )
, Micro
    ( Code
        ( IfNotEq
            ( Literal 1 ) "a" []
        )
    )
, Micro
    ( Code
        ( Msg [ "stones." ] )
    )
, Micro ( Code End )
, Micro ( Code Line )
, Micro ( Code Line )
, Micro
    ( Code
        ( IfEq
            ( Literal 0 ) "stones" []
        )
    )
, Micro
    ( Code
        ( Msg
            [ "HAHAHAHAHAHAHAHAHA!!!"
            , "I"
            , "WON!"
            , "LOOOOOOOSSSEEEEERRRRRRRRRRRRRRRRR.........."
            ]
        )
    )
, Micro ( Code Line )
, Micro ( Code End )
, Micro
    ( Code
        ( Set 2 "humanturn" )
    )
, Micro ( Code End )
, Micro
    ( Code
        ( IfEq
            ( Literal 1 ) "humanturn" []
        )
    )
, Micro
    ( Code
        ( Msg
            [ "How"
            , "many"
            , "stones"
            , "will"
            , "you"
            , "draw?"
            ]
        )
    )
, Micro ( Code Line )
, Micro
    ( Code
        ( Read "a" )
    )
, Micro ( Code Line )
, Micro
    ( Code
        ( Dec 48 "a" )
    )
, Micro
    ( Code
        ( Comp
            ( Variable "stones" )
            ( Variable "a" ) "b"
        )
    )
, Micro
    ( Code
        ( Comp
            ( Variable "a" )
            ( Variable "max" ) "c"
        )
    )
, Micro
    ( Code
        ( IfEq
            ( Literal 0 ) "a" []
        )
    )
, Micro
    ( Code
        ( Msg
            [ "You"
            , "must"
            , "take"
            , "at"
            , "least"
            , "one"
            , "stone!"
            ]
        )
    )
, Micro ( Code Line )
, Micro ( Code End )
, Micro
    ( Code
        ( IfNotEq
            ( Literal 0 ) "a" []
        )
    )
, Micro
    ( Code
        ( IfNotEq
            ( Literal 2 ) "b" []
        )
    )
, Micro
    ( Code
        ( IfEq
            ( Literal 0 ) "c" []
        )
    )
, Micro
    ( Code
        ( Msg
            [ "You"
            , "cannot"
            , "take"
            , "that"
            , "many"
            , "stones!"
            ]
        )
    )
, Micro ( Code Line )
, Micro ( Code End )
, Micro
    ( Code
        ( IfNotEq
            ( Literal 0 ) "c" []
        )
    )
, Micro
    ( Code
        ( Sub
            ( Variable "stones" )
            ( Variable "a" ) "stones"
        )
    )
, Micro
    ( Code
        ( Set 0 "humanturn" )
    )
, Micro ( Code End )
, Micro ( Code End )
, Micro ( Code End )
, Micro
    ( Code
        ( IfEq
            ( Literal 2 ) "b" []
        )
    )
, Micro
    ( Code
        ( Msg
            [ "Yeah"
            , "sure..."
            ]
        )
    )
, Micro ( Code Line )
, Micro ( Code End )
, Micro
    ( Code
        ( IfEq
            ( Literal 0 ) "stones" []
        )
    )
, Micro
    ( Code
        ( Msg
            [ "Oh"
            , "shit..."
            , "You"
            , "won..."
            , "Stupid"
            , "cheater..."
            ]
        )
    )
, Micro ( Code Line )
, Micro ( Code End )
, Micro ( Code Line )
, Micro ( Code End )
, Micro
    ( Code
        ( IfEq
            ( Literal 2 ) "humanturn" []
        )
    )
, Micro
    ( Code
        ( Set 1 "humanturn" )
    )
, Micro ( Code End )
, Micro ( Code End )
, Micro ( Code Line )
, Micro
    ( Code
        ( Msg
            [ "This"
            , "program"
            , "was"
            , "written"
            , "in"
            , "FBF"
            , "by"
            , "Tritonio."
            , "http://students.ceid.upatras.gr/~asimakis/FBF.html"
            ]
        )
    )
, Micro ( Code Line )
]