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
    ( Call "ifnoteq"
        [ Variable "a"
        , Literal 10
        ]
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
    ( Call "ifnoteq"
        [ Variable "a"
        , Literal 10
        ]
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
    ( Call "end" [] )
, Code
    ( Call "end" [] )
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
    ( Call "ifeq"
        [ Variable "a"
        , Literal 110
        ]
    )
, Code
    ( Set 1 "humanturn" )
, Code
    ( Call "end" [] )
, Code Line
, Code Line
, Code
    ( Call "uneq"
        [ Variable "stones"
        , Literal 0
        ]
    )
, Code
    ( Msg [ "Stones:" ] )
, Code Space
, Code
    ( Copy "stones" "drawstone" )
, Code
    ( Call "uneq"
        [ Variable "drawstone"
        , Literal 0
        ]
    )
, Code
    ( Msg [ "O" ] )
, Code
    ( Dec 1 "drawstone" )
, Code
    ( Call "end" [] )
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
    ( Call "ifnoteq"
        [ Variable "a"
        , Literal 48
        ]
    )
, Code
    ( Print [ "a" ] )
, Code
    ( Call "end" [] )
, Code
    ( Call "ifeq"
        [ Variable "b"
        , Literal 48
        ]
    )
, Code
    ( Call "ifnoteq"
        [ Variable "a"
        , Literal 48
        ]
    )
, Code
    ( Print [ "b" ] )
, Code
    ( Call "end" [] )
, Code
    ( Call "end" [] )
, Code
    ( Call "ifnoteq"
        [ Variable "b"
        , Literal 48
        ]
    )
, Code
    ( Print [ "b" ] )
, Code
    ( Call "end" [] )
, Code
    ( Print [ "c" ] )
, Code Line
, Code
    ( Call "ifeq"
        [ Variable "humanturn"
        , Literal 0
        ]
    )
, Code
    ( Mod
        ( Variable "stones" )
        ( Variable "maxplusone" ) "a"
    )
, Code
    ( Call "ifeq"
        [ Variable "a"
        , Literal 0
        ]
    )
, Code
    ( Set 1 "a" )
, Code
    ( Call "end" [] )
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
    ( Call "ifeq"
        [ Variable "a"
        , Literal 1
        ]
    )
, Code
    ( Msg [ "stone." ] )
, Code
    ( Call "end" [] )
, Code
    ( Call "ifnoteq"
        [ Variable "a"
        , Literal 1
        ]
    )
, Code
    ( Msg [ "stones." ] )
, Code
    ( Call "end" [] )
, Code Line
, Code Line
, Code
    ( Call "ifeq"
        [ Variable "stones"
        , Literal 0
        ]
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
, Code
    ( Call "end" [] )
, Code
    ( Set 2 "humanturn" )
, Code
    ( Call "end" [] )
, Code
    ( Call "ifeq"
        [ Variable "humanturn"
        , Literal 1
        ]
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
    ( Call "ifeq"
        [ Variable "a"
        , Literal 0
        ]
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
, Code
    ( Call "end" [] )
, Code
    ( Call "ifnoteq"
        [ Variable "a"
        , Literal 0
        ]
    )
, Code
    ( Call "ifnoteq"
        [ Variable "b"
        , Literal 2
        ]
    )
, Code
    ( Call "ifeq"
        [ Variable "c"
        , Literal 0
        ]
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
, Code
    ( Call "end" [] )
, Code
    ( Call "ifnoteq"
        [ Variable "c"
        , Literal 0
        ]
    )
, Code
    ( Sub
        ( Variable "stones" )
        ( Variable "a" ) "stones"
    )
, Code
    ( Set 0 "humanturn" )
, Code
    ( Call "end" [] )
, Code
    ( Call "end" [] )
, Code
    ( Call "end" [] )
, Code
    ( Call "ifeq"
        [ Variable "b"
        , Literal 2
        ]
    )
, Code
    ( Msg
        [ "Yeah"
        , "sure..."
        ]
    )
, Code Line
, Code
    ( Call "end" [] )
, Code
    ( Call "ifeq"
        [ Variable "stones"
        , Literal 0
        ]
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
, Code
    ( Call "end" [] )
, Code Line
, Code
    ( Call "end" [] )
, Code
    ( Call "ifeq"
        [ Variable "humanturn"
        , Literal 2
        ]
    )
, Code
    ( Set 1 "humanturn" )
, Code
    ( Call "end" [] )
, Code
    ( Call "end" [] )
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