[ Compiler
    ( LineBreaks 80 )
, Compiler
    ( Dim [ "stones" ] )
, Compiler
    ( Dim [ "humanturn" ] )
, Compiler
    ( Dim [ "a" ] )
, Compiler
    ( Dim [ "b" ] )
, Compiler
    ( Dim [ "c" ] )
, Compiler
    ( Dim [ "max" ] )
, Compiler
    ( Dim [ "maxplusone" ] )
, Compiler
    ( Dim [ "drawstone" ] )
, Code Line
, Code
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
, Code Line
, Code
    ( Read "stones" )
, Code
    ( Dec 48 "stones" )
, Code
    ( Read "a" )
, Code
    ( IfNotEq
        ( Literal 10 ) "a" []
    )
, Code
    ( Multi
        ( Variable "stones" )
        ( Literal 10 ) "stones"
    )
, Code
    ( Dec 48 "a" )
, Code
    ( Add
        ( Variable "stones" )
        ( Variable "a" ) "stones"
    )
, Code
    ( Read "a" )
, Code
    ( IfNotEq
        ( Literal 10 ) "a" []
    )
, Code
    ( Multi
        ( Variable "stones" )
        ( Literal 10 ) "stones"
    )
, Code
    ( Dec 48 "a" )
, Code
    ( Add
        ( Variable "stones" )
        ( Variable "a" ) "stones"
    )
, Code
    ( Read "a" )
, Code End
, Code End
, Code Line
, Code
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
, Code Line
, Code
    ( Read "max" )
, Code
    ( Dec 48 "max" )
, Code
    ( Add
        ( Variable "max" )
        ( Literal 1 ) "maxplusone"
    )
, Code Line
, Code Line
, Code
    ( Msg
        [ "Shall"
        , "I"
        , "go"
        , "first?"
        , "(y/n)"
        ]
    )
, Code Line
, Code
    ( Read "a" )
, Code
    ( IfEq
        ( Literal 110 ) "a" []
    )
, Code
    ( Set 1 "humanturn" )
, Code End
, Code Line
, Code Line
, Code
    ( UnEq
        ( Literal 0 ) "stones" []
    )
, Code
    ( Msg [ "Stones:" ] )
, Code Space
, Code
    ( Copy "stones" "drawstone" )
, Code
    ( UnEq
        ( Literal 0 ) "drawstone" []
    )
, Code
    ( Msg [ "O" ] )
, Code
    ( Dec 1 "drawstone" )
, Code End
, Code Space
, Code
    ( Msg [ "=" ] )
, Code Space
, Code
    ( Div
        ( Variable "stones" )
        ( Literal 100 ) "a"
    )
, Code
    ( Mod
        ( Variable "stones" )
        ( Literal 100 ) "b"
    )
, Code
    ( Mod
        ( Variable "b" )
        ( Literal 10 ) "c"
    )
, Code
    ( Div
        ( Variable "b" )
        ( Literal 10 ) "b"
    )
, Code
    ( Inc 48 "a" )
, Code
    ( Inc 48 "b" )
, Code
    ( Inc 48 "c" )
, Code
    ( IfNotEq
        ( Literal 48 ) "a" []
    )
, Code
    ( Print [ "a" ] )
, Code End
, Code
    ( IfEq
        ( Literal 48 ) "b" []
    )
, Code
    ( IfNotEq
        ( Literal 48 ) "a" []
    )
, Code
    ( Print [ "b" ] )
, Code End
, Code End
, Code
    ( IfNotEq
        ( Literal 48 ) "b" []
    )
, Code
    ( Print [ "b" ] )
, Code End
, Code
    ( Print [ "c" ] )
, Code Line
, Code
    ( IfEq
        ( Literal 0 ) "humanturn" []
    )
, Code
    ( Mod
        ( Variable "stones" )
        ( Variable "maxplusone" ) "a"
    )
, Code
    ( IfEq
        ( Literal 0 ) "a" []
    )
, Code
    ( Set 1 "a" )
, Code End
, Code
    ( Sub
        ( Variable "stones" )
        ( Variable "a" ) "stones"
    )
, Code
    ( Msg
        [ "I"
        , "will"
        , "draw"
        ]
    )
, Code Space
, Code
    ( Inc 48 "a" )
, Code
    ( Print [ "a" ] )
, Code Space
, Code
    ( Dec 48 "a" )
, Code
    ( IfEq
        ( Literal 1 ) "a" []
    )
, Code
    ( Msg [ "stone." ] )
, Code End
, Code
    ( IfNotEq
        ( Literal 1 ) "a" []
    )
, Code
    ( Msg [ "stones." ] )
, Code End
, Code Line
, Code Line
, Code
    ( IfEq
        ( Literal 0 ) "stones" []
    )
, Code
    ( Msg
        [ "HAHAHAHAHAHAHAHAHA!!!"
        , "I"
        , "WON!"
        , "LOOOOOOOSSSEEEEERRRRRRRRRRRRRRRRR.........."
        ]
    )
, Code Line
, Code End
, Code
    ( Set 2 "humanturn" )
, Code End
, Code
    ( IfEq
        ( Literal 1 ) "humanturn" []
    )
, Code
    ( Msg
        [ "How"
        , "many"
        , "stones"
        , "will"
        , "you"
        , "draw?"
        ]
    )
, Code Line
, Code
    ( Read "a" )
, Code Line
, Code
    ( Dec 48 "a" )
, Code
    ( Comp
        ( Variable "stones" )
        ( Variable "a" ) "b"
    )
, Code
    ( Comp
        ( Variable "a" )
        ( Variable "max" ) "c"
    )
, Code
    ( IfEq
        ( Literal 0 ) "a" []
    )
, Code
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
, Code Line
, Code End
, Code
    ( IfNotEq
        ( Literal 0 ) "a" []
    )
, Code
    ( IfNotEq
        ( Literal 2 ) "b" []
    )
, Code
    ( IfEq
        ( Literal 0 ) "c" []
    )
, Code
    ( Msg
        [ "You"
        , "cannot"
        , "take"
        , "that"
        , "many"
        , "stones!"
        ]
    )
, Code Line
, Code End
, Code
    ( IfNotEq
        ( Literal 0 ) "c" []
    )
, Code
    ( Sub
        ( Variable "stones" )
        ( Variable "a" ) "stones"
    )
, Code
    ( Set 0 "humanturn" )
, Code End
, Code End
, Code End
, Code
    ( IfEq
        ( Literal 2 ) "b" []
    )
, Code
    ( Msg
        [ "Yeah"
        , "sure..."
        ]
    )
, Code Line
, Code End
, Code
    ( IfEq
        ( Literal 0 ) "stones" []
    )
, Code
    ( Msg
        [ "Oh"
        , "shit..."
        , "You"
        , "won..."
        , "Stupid"
        , "cheater..."
        ]
    )
, Code Line
, Code End
, Code Line
, Code End
, Code
    ( IfEq
        ( Literal 2 ) "humanturn" []
    )
, Code
    ( Set 1 "humanturn" )
, Code End
, Code End
, Code Line
, Code
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
, Code Line
]