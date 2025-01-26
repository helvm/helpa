[ Micro
    ( Compiler
        ( LineBreaks 80 )
    )
, Micro
    ( Compiler
        ( Dim [ "room" ] )
    )
, Micro
    ( Compiler
        ( Dim [ "oldroom" ] )
    )
, Micro
    ( Compiler
        ( Dim [ "has_green" ] )
    )
, Micro
    ( Compiler
        ( Dim [ "has_sword" ] )
    )
, Micro
    ( Compiler
        ( Dim [ "has_red" ] )
    )
, Micro
    ( Compiler
        ( Dim [ "playing" ] )
    )
, Micro
    ( Compiler
        ( Dim [ "temp" ] )
    )
, Micro
    ( Code
        ( Set 1 "playing" )
    )
, Micro
    ( Code
        ( Set 1 "room" )
    )
, Micro ( Code Line )
, Micro ( Code Line )
, Micro ( Code Line )
, Micro
    ( Code
        ( Msg
            [ "Welcome"
            , "to"
            , "Groop."
            ]
        )
    )
, Micro ( Code Line )
, Micro
    ( Code
        ( Msg
            [ "This"
            , "game"
            , "was"
            , "written"
            , "in"
            , "FBF"
            , "and"
            , "then"
            , "compiled"
            , "to"
            , "brainfuck"
            , "with"
            , "the"
            , "FBF"
            , "compiler"
            , "by"
            , "Tritonio."
            , "This"
            , "is"
            , "just"
            , "a"
            , "demo"
            , "version."
            ]
        )
    )
, Micro ( Code Line )
, Micro
    ( Code
        ( Msg
            [ "You"
            , "must"
            , "get"
            , "out"
            , "before"
            , "the"
            , "come"
            , "for"
            , "you!"
            ]
        )
    )
, Micro ( Code Line )
, Micro
    ( Code
        ( UnEq
            ( Literal 0 ) "playing" []
        )
    )
, Micro
    ( Code
        ( IfEq
            ( Literal 0 ) "room" []
        )
    )
, Micro ( Code Line )
, Micro
    ( Code
        ( Msg
            [ "***"
            , "MENU"
            , "***"
            ]
        )
    )
, Micro ( Code Line )
, Micro
    ( Code
        ( Msg
            [ "1."
            , "Exit"
            , "Game"
            ]
        )
    )
, Micro ( Code Line )
, Micro
    ( Code
        ( Msg
            [ "2."
            , "Continue"
            , "Game"
            ]
        )
    )
, Micro ( Code Line )
, Micro
    ( Code
        ( IfNotEq
            ( Literal 0 ) "has_red" []
        )
    )
, Micro
    ( Code
        ( Msg
            [ "You"
            , "have"
            , "a"
            , "red"
            , "key."
            ]
        )
    )
, Micro ( Code Line )
, Micro ( Code End )
, Micro
    ( Code
        ( IfNotEq
            ( Literal 0 ) "has_green" []
        )
    )
, Micro
    ( Code
        ( Msg
            [ "You"
            , "have"
            , "a"
            , "green"
            , "key."
            ]
        )
    )
, Micro ( Code Line )
, Micro ( Code End )
, Micro
    ( Code
        ( IfNotEq
            ( Literal 0 ) "has_sword" []
        )
    )
, Micro
    ( Code
        ( Msg
            [ "You"
            , "have"
            , "a"
            , "sword."
            ]
        )
    )
, Micro ( Code Line )
, Micro ( Code End )
, Micro
    ( Code
        ( Read "temp" )
    )
, Micro
    ( Code
        ( Dec 48 "temp" )
    )
, Micro
    ( Code
        ( IfEq
            ( Literal 1 ) "temp" []
        )
    )
, Micro
    ( Code
        ( Set 0 "playing" )
    )
, Micro ( Code End )
, Micro
    ( Code
        ( IfEq
            ( Literal 2 ) "temp" []
        )
    )
, Micro
    ( Code
        ( Copy "oldroom" "room" )
    )
, Micro ( Code End )
, Micro
    ( Code
        ( IfEq
            ( Literal 8 ) "temp" []
        )
    )
, Micro
    ( Code
        ( Read "room" )
    )
, Micro
    ( Code
        ( Dec 48 "room" )
    )
, Micro ( Code End )
, Micro ( Code End )
, Micro
    ( Code
        ( IfEq
            ( Literal 1 ) "room" []
        )
    )
, Micro ( Code Line )
, Micro
    ( Code
        ( Msg
            [ "You"
            , "are"
            , "in"
            , "the"
            , "entrance"
            , "room"
            , "of"
            , "an"
            , "old"
            , "house."
            , "The"
            , "huge"
            , "door"
            , "to"
            , "the"
            , "south"
            , "is"
            , "jammed."
            , "There"
            , "is"
            , "a"
            , "door"
            , "to"
            , "the"
            , "north"
            , "and"
            , "one"
            , "more"
            , "to"
            , "the"
            , "east."
            ]
        )
    )
, Micro ( Code Line )
, Micro
    ( Code
        ( Msg
            [ "1."
            , "Go"
            , "north"
            , "through"
            , "the"
            , "door."
            ]
        )
    )
, Micro ( Code Line )
, Micro
    ( Code
        ( Msg
            [ "2."
            , "Go"
            , "east"
            , "through"
            , "the"
            , "door."
            ]
        )
    )
, Micro ( Code Line )
, Micro
    ( Code
        ( Msg
            [ "0."
            , "Menu"
            ]
        )
    )
, Micro ( Code Line )
, Micro ( Code Line )
, Micro
    ( Code
        ( Read "temp" )
    )
, Micro
    ( Code
        ( Dec 48 "temp" )
    )
, Micro
    ( Code
        ( IfEq
            ( Literal 0 ) "temp" []
        )
    )
, Micro
    ( Code
        ( Copy "room" "oldroom" )
    )
, Micro
    ( Code
        ( Set 0 "room" )
    )
, Micro ( Code End )
, Micro
    ( Code
        ( IfEq
            ( Literal 1 ) "temp" []
        )
    )
, Micro
    ( Code
        ( Set 9 "room" )
    )
, Micro
    ( Code
        ( Msg
            [ "You"
            , "went"
            , "north."
            ]
        )
    )
, Micro ( Code End )
, Micro
    ( Code
        ( IfEq
            ( Literal 2 ) "temp" []
        )
    )
, Micro
    ( Code
        ( Set 6 "room" )
    )
, Micro
    ( Code
        ( Msg
            [ "You"
            , "went"
            , "east."
            ]
        )
    )
, Micro ( Code End )
, Micro ( Code Line )
, Micro ( Code End )
, Micro
    ( Code
        ( IfEq
            ( Literal 2 ) "room" []
        )
    )
, Micro ( Code Line )
, Micro
    ( Code
        ( Msg
            [ "You"
            , "are"
            , "in"
            , "a"
            , "kitchen."
            , "There"
            , "is"
            , "a"
            , "very"
            , "small"
            , "closet"
            , "to"
            , "the"
            , "west."
            , "There"
            , "is"
            , "a"
            , "door"
            , "to"
            , "the"
            , "east."
            ]
        )
    )
, Micro ( Code Line )
, Micro
    ( Code
        ( Msg
            [ "1."
            , "Open"
            , "the"
            , "closet."
            ]
        )
    )
, Micro ( Code Line )
, Micro
    ( Code
        ( Msg
            [ "2."
            , "Go"
            , "east"
            , "through"
            , "the"
            , "door."
            ]
        )
    )
, Micro ( Code Line )
, Micro
    ( Code
        ( Msg
            [ "0."
            , "Menu"
            ]
        )
    )
, Micro ( Code Line )
, Micro ( Code Line )
, Micro
    ( Code
        ( Read "temp" )
    )
, Micro
    ( Code
        ( Dec 48 "temp" )
    )
, Micro
    ( Code
        ( IfEq
            ( Literal 0 ) "temp" []
        )
    )
, Micro
    ( Code
        ( Copy "room" "oldroom" )
    )
, Micro
    ( Code
        ( Set 0 "room" )
    )
, Micro ( Code End )
, Micro
    ( Code
        ( IfEq
            ( Literal 1 ) "temp" []
        )
    )
, Micro
    ( Code
        ( IfEq
            ( Literal 1 ) "has_green" []
        )
    )
, Micro
    ( Code
        ( Msg
            [ "There"
            , "is"
            , "nothing"
            , "more"
            , "in"
            , "the"
            , "closet."
            ]
        )
    )
, Micro ( Code End )
, Micro
    ( Code
        ( IfEq
            ( Literal 0 ) "has_green" []
        )
    )
, Micro
    ( Code
        ( Set 1 "has_green" )
    )
, Micro
    ( Code
        ( Msg
            [ "You"
            , "found"
            , "a"
            , "rusty"
            , "green"
            , "key"
            , "in"
            , "the"
            , "closet!"
            , "You"
            , "took"
            , "it."
            ]
        )
    )
, Micro ( Code End )
, Micro ( Code End )
, Micro
    ( Code
        ( IfEq
            ( Literal 2 ) "temp" []
        )
    )
, Micro
    ( Code
        ( Set 9 "room" )
    )
, Micro
    ( Code
        ( Msg
            [ "You"
            , "went"
            , "east."
            ]
        )
    )
, Micro ( Code End )
, Micro ( Code Line )
, Micro ( Code End )
, Micro
    ( Code
        ( IfEq
            ( Literal 3 ) "room" []
        )
    )
, Micro ( Code Line )
, Micro
    ( Code
        ( IfEq
            ( Literal 1 ) "has_red" []
        )
    )
, Micro
    ( Code
        ( Msg
            [ "You"
            , "are"
            , "in"
            , "a"
            , "small"
            , "cellar."
            , "There"
            , "are"
            , "wine"
            , "bottles"
            , "everywhere."
            ]
        )
    )
, Micro ( Code Line )
, Micro
    ( Code
        ( Msg
            [ "1."
            , "Go"
            , "north"
            , "through"
            , "the"
            , "door."
            ]
        )
    )
, Micro ( Code Line )
, Micro
    ( Code
        ( Msg
            [ "0."
            , "Menu"
            ]
        )
    )
, Micro ( Code Line )
, Micro ( Code Line )
, Micro
    ( Code
        ( Read "temp" )
    )
, Micro
    ( Code
        ( Dec 48 "temp" )
    )
, Micro
    ( Code
        ( IfEq
            ( Literal 0 ) "temp" []
        )
    )
, Micro
    ( Code
        ( Copy "room" "oldroom" )
    )
, Micro
    ( Code
        ( Set 0 "room" )
    )
, Micro ( Code End )
, Micro
    ( Code
        ( IfEq
            ( Literal 1 ) "temp" []
        )
    )
, Micro
    ( Code
        ( Set 7 "room" )
    )
, Micro
    ( Code
        ( Msg
            [ "You"
            , "went"
            , "north."
            ]
        )
    )
, Micro ( Code End )
, Micro ( Code End )
, Micro
    ( Code
        ( IfEq
            ( Literal 0 ) "has_red" []
        )
    )
, Micro
    ( Code
        ( Msg
            [ "You"
            , "are"
            , "in"
            , "a"
            , "small"
            , "cellar."
            , "There"
            , "are"
            , "wine"
            , "bottles"
            , "everywhere."
            , "There"
            , "is"
            , "something"
            , "in"
            , "one"
            , "of"
            , "the"
            , "bottles."
            ]
        )
    )
, Micro ( Code Line )
, Micro
    ( Code
        ( Msg
            [ "1."
            , "Go"
            , "north"
            , "through"
            , "the"
            , "door."
            ]
        )
    )
, Micro ( Code Line )
, Micro
    ( Code
        ( Msg
            [ "2."
            , "Break"
            , "the"
            , "bottle."
            ]
        )
    )
, Micro ( Code Line )
, Micro
    ( Code
        ( Msg
            [ "0."
            , "Menu"
            ]
        )
    )
, Micro ( Code Line )
, Micro ( Code Line )
, Micro
    ( Code
        ( Read "temp" )
    )
, Micro
    ( Code
        ( Dec 48 "temp" )
    )
, Micro
    ( Code
        ( IfEq
            ( Literal 0 ) "temp" []
        )
    )
, Micro
    ( Code
        ( Copy "room" "oldroom" )
    )
, Micro
    ( Code
        ( Set 0 "room" )
    )
, Micro ( Code End )
, Micro
    ( Code
        ( IfEq
            ( Literal 1 ) "temp" []
        )
    )
, Micro
    ( Code
        ( Set 7 "room" )
    )
, Micro
    ( Code
        ( Msg
            [ "You"
            , "went"
            , "north."
            ]
        )
    )
, Micro ( Code End )
, Micro
    ( Code
        ( IfEq
            ( Literal 2 ) "temp" []
        )
    )
, Micro
    ( Code
        ( Msg
            [ "You"
            , "broke"
            , "the"
            , "bottle"
            , "and"
            , "found"
            , "a"
            , "red"
            , "key!"
            , "You"
            , "took"
            , "it!"
            ]
        )
    )
, Micro
    ( Code
        ( Set 1 "has_red" )
    )
, Micro ( Code End )
, Micro ( Code End )
, Micro ( Code Line )
, Micro ( Code End )
, Micro
    ( Code
        ( IfEq
            ( Literal 4 ) "room" []
        )
    )
, Micro ( Code Line )
, Micro
    ( Code
        ( Msg
            [ "You"
            , "are"
            , "in"
            , "the"
            , "servants"
            , "bedroom."
            , "There"
            , "is"
            , "a"
            , "door"
            , "to"
            , "the"
            , "west"
            , "and"
            , "a"
            , "blue"
            , "one"
            , "to"
            , "the"
            , "east."
            ]
        )
    )
, Micro ( Code Line )
, Micro
    ( Code
        ( Msg
            [ "1."
            , "Go"
            , "east"
            , "through"
            , "the"
            , "door."
            ]
        )
    )
, Micro ( Code Line )
, Micro
    ( Code
        ( Msg
            [ "2."
            , "Go"
            , "west"
            , "through"
            , "the"
            , "door."
            ]
        )
    )
, Micro ( Code Line )
, Micro
    ( Code
        ( Msg
            [ "0."
            , "Menu"
            ]
        )
    )
, Micro ( Code Line )
, Micro ( Code Line )
, Micro
    ( Code
        ( Read "temp" )
    )
, Micro
    ( Code
        ( Dec 48 "temp" )
    )
, Micro
    ( Code
        ( IfEq
            ( Literal 0 ) "temp" []
        )
    )
, Micro
    ( Code
        ( Copy "room" "oldroom" )
    )
, Micro
    ( Code
        ( Set 0 "room" )
    )
, Micro ( Code End )
, Micro
    ( Code
        ( IfEq
            ( Literal 1 ) "temp" []
        )
    )
, Micro
    ( Code
        ( Set 6 "room" )
    )
, Micro
    ( Code
        ( Msg
            [ "You"
            , "went"
            , "east."
            ]
        )
    )
, Micro ( Code End )
, Micro
    ( Code
        ( IfEq
            ( Literal 2 ) "temp" []
        )
    )
, Micro
    ( Code
        ( Set 9 "room" )
    )
, Micro
    ( Code
        ( Msg
            [ "You"
            , "went"
            , "west."
            ]
        )
    )
, Micro ( Code End )
, Micro ( Code Line )
, Micro ( Code End )
, Micro
    ( Code
        ( IfEq
            ( Literal 5 ) "room" []
        )
    )
, Micro ( Code Line )
, Micro
    ( Code
        ( IfEq
            ( Literal 0 ) "has_sword" []
        )
    )
, Micro
    ( Code
        ( Msg
            [ "You"
            , "are"
            , "in"
            , "the"
            , "back"
            , "yard."
            , "A"
            , "guard"
            , "sees"
            , "you."
            , "You"
            , "try"
            , "to"
            , "run"
            , "but"
            , "he"
            , "strikes"
            , "you"
            , "with"
            , "his"
            , "sword."
            , "You"
            , "fall"
            , "dead."
            ]
        )
    )
, Micro ( Code End )
, Micro
    ( Code
        ( IfEq
            ( Literal 1 ) "has_sword" []
        )
    )
, Micro
    ( Code
        ( Msg
            [ "You"
            , "are"
            , "in"
            , "the"
            , "back"
            , "yard."
            , "A"
            , "guard"
            , "sees"
            , "you."
            , "He"
            , "tries"
            , "to"
            , "hit"
            , "you"
            , "but"
            , "you"
            , "defend"
            , "with"
            , "your"
            , "sword"
            , "and"
            , "fight"
            , "back."
            , "You"
            , "finally"
            , "beat"
            , "the"
            , "guard."
            , "Congratulations!"
            , "You"
            , "are"
            , "free"
            , "now!"
            , "Thank"
            , "you"
            , "for"
            , "playing"
            ]
        )
    )
, Micro ( Code End )
, Micro ( Code Line )
, Micro
    ( Code
        ( Set 0 "playing" )
    )
, Micro ( Code End )
, Micro
    ( Code
        ( IfEq
            ( Literal 6 ) "room" []
        )
    )
, Micro ( Code Line )
, Micro
    ( Code
        ( IfEq
            ( Literal 0 ) "has_green" []
        )
    )
, Micro
    ( Code
        ( Msg
            [ "You"
            , "are"
            , "in"
            , "a"
            , "huge"
            , "bedroom."
            , "There"
            , "are"
            , "two"
            , "doors"
            , "to"
            , "the"
            , "west:"
            , "a"
            , "blue"
            , "one"
            , "and"
            , "a"
            , "small"
            , "wooden"
            , "door."
            , "There"
            , "is"
            , "also"
            , "a"
            , "locked"
            , "door"
            , "on"
            , "the"
            , "floor."
            ]
        )
    )
, Micro ( Code Line )
, Micro
    ( Code
        ( Msg
            [ "1."
            , "Go"
            , "west"
            , "through"
            , "the"
            , "blue"
            , "door."
            ]
        )
    )
, Micro ( Code Line )
, Micro
    ( Code
        ( Msg
            [ "2."
            , "Go"
            , "west"
            , "through"
            , "the"
            , "small"
            , "wooden"
            , "door."
            ]
        )
    )
, Micro ( Code Line )
, Micro
    ( Code
        ( Msg
            [ "0."
            , "Menu"
            ]
        )
    )
, Micro ( Code Line )
, Micro ( Code Line )
, Micro
    ( Code
        ( Read "temp" )
    )
, Micro
    ( Code
        ( Dec 48 "temp" )
    )
, Micro
    ( Code
        ( IfEq
            ( Literal 0 ) "temp" []
        )
    )
, Micro
    ( Code
        ( Copy "room" "oldroom" )
    )
, Micro
    ( Code
        ( Set 0 "room" )
    )
, Micro ( Code End )
, Micro
    ( Code
        ( IfEq
            ( Literal 1 ) "temp" []
        )
    )
, Micro
    ( Code
        ( Set 4 "room" )
    )
, Micro
    ( Code
        ( Msg
            [ "You"
            , "went"
            , "west"
            , "through"
            , "the"
            , "blue"
            , "door."
            ]
        )
    )
, Micro ( Code End )
, Micro
    ( Code
        ( IfEq
            ( Literal 2 ) "temp" []
        )
    )
, Micro
    ( Code
        ( Set 1 "room" )
    )
, Micro
    ( Code
        ( Msg
            [ "You"
            , "went"
            , "west"
            , "through"
            , "a"
            , "small"
            , "wooden"
            , "door."
            ]
        )
    )
, Micro ( Code End )
, Micro ( Code End )
, Micro
    ( Code
        ( IfEq
            ( Literal 2 ) "has_green" []
        )
    )
, Micro
    ( Code
        ( Msg
            [ "You"
            , "are"
            , "in"
            , "a"
            , "huge"
            , "bedroom."
            , "There"
            , "are"
            , "two"
            , "doors"
            , "to"
            , "the"
            , "west:"
            , "a"
            , "blue"
            , "one"
            , "and"
            , "a"
            , "small"
            , "wooden"
            , "door."
            , "There"
            , "is"
            , "also"
            , "a"
            , "door"
            , "on"
            , "the"
            , "floor."
            ]
        )
    )
, Micro ( Code Line )
, Micro
    ( Code
        ( Msg
            [ "1."
            , "Go"
            , "west"
            , "through"
            , "the"
            , "blue"
            , "door."
            ]
        )
    )
, Micro ( Code Line )
, Micro
    ( Code
        ( Msg
            [ "2."
            , "Go"
            , "west"
            , "through"
            , "the"
            , "small"
            , "wooden"
            , "door."
            ]
        )
    )
, Micro ( Code Line )
, Micro
    ( Code
        ( Msg
            [ "3."
            , "Go"
            , "down"
            , "through"
            , "the"
            , "floor"
            , "door."
            ]
        )
    )
, Micro ( Code Line )
, Micro
    ( Code
        ( Msg
            [ "0."
            , "Menu"
            ]
        )
    )
, Micro ( Code Line )
, Micro ( Code Line )
, Micro
    ( Code
        ( Read "temp" )
    )
, Micro
    ( Code
        ( Dec 48 "temp" )
    )
, Micro
    ( Code
        ( IfEq
            ( Literal 0 ) "temp" []
        )
    )
, Micro
    ( Code
        ( Copy "room" "oldroom" )
    )
, Micro
    ( Code
        ( Set 0 "room" )
    )
, Micro ( Code End )
, Micro
    ( Code
        ( IfEq
            ( Literal 1 ) "temp" []
        )
    )
, Micro
    ( Code
        ( Set 4 "room" )
    )
, Micro
    ( Code
        ( Msg
            [ "You"
            , "went"
            , "west"
            , "through"
            , "the"
            , "blue"
            , "door."
            ]
        )
    )
, Micro ( Code End )
, Micro
    ( Code
        ( IfEq
            ( Literal 2 ) "temp" []
        )
    )
, Micro
    ( Code
        ( Set 1 "room" )
    )
, Micro
    ( Code
        ( Msg
            [ "You"
            , "went"
            , "west"
            , "through"
            , "a"
            , "small"
            , "wooden"
            , "door."
            ]
        )
    )
, Micro ( Code End )
, Micro
    ( Code
        ( IfEq
            ( Literal 3 ) "temp" []
        )
    )
, Micro
    ( Code
        ( Set 7 "room" )
    )
, Micro
    ( Code
        ( Msg
            [ "You"
            , "went"
            , "down."
            ]
        )
    )
, Micro ( Code End )
, Micro ( Code End )
, Micro
    ( Code
        ( IfEq
            ( Literal 1 ) "has_green" []
        )
    )
, Micro
    ( Code
        ( Msg
            [ "You"
            , "are"
            , "in"
            , "a"
            , "huge"
            , "bedroom."
            , "There"
            , "are"
            , "two"
            , "doors"
            , "to"
            , "the"
            , "west:"
            , "a"
            , "blue"
            , "one"
            , "and"
            , "a"
            , "small"
            , "wooden"
            , "door."
            , "There"
            , "is"
            , "also"
            , "a"
            , "locked"
            , "door"
            , "on"
            , "the"
            , "floor."
            ]
        )
    )
, Micro ( Code Line )
, Micro
    ( Code
        ( Msg
            [ "1."
            , "Go"
            , "west"
            , "through"
            , "the"
            , "blue"
            , "door."
            ]
        )
    )
, Micro ( Code Line )
, Micro
    ( Code
        ( Msg
            [ "2."
            , "Go"
            , "west"
            , "through"
            , "the"
            , "small"
            , "wooden"
            , "door."
            ]
        )
    )
, Micro ( Code Line )
, Micro
    ( Code
        ( Msg
            [ "3."
            , "Unlock"
            , "the"
            , "floor"
            , "door."
            ]
        )
    )
, Micro ( Code Line )
, Micro
    ( Code
        ( Msg
            [ "0."
            , "Menu"
            ]
        )
    )
, Micro ( Code Line )
, Micro ( Code Line )
, Micro
    ( Code
        ( Read "temp" )
    )
, Micro
    ( Code
        ( Dec 48 "temp" )
    )
, Micro
    ( Code
        ( IfEq
            ( Literal 0 ) "temp" []
        )
    )
, Micro
    ( Code
        ( Copy "room" "oldroom" )
    )
, Micro
    ( Code
        ( Set 0 "room" )
    )
, Micro ( Code End )
, Micro
    ( Code
        ( IfEq
            ( Literal 1 ) "temp" []
        )
    )
, Micro
    ( Code
        ( Set 4 "room" )
    )
, Micro
    ( Code
        ( Msg
            [ "You"
            , "went"
            , "west"
            , "through"
            , "the"
            , "blue"
            , "door."
            ]
        )
    )
, Micro ( Code End )
, Micro
    ( Code
        ( IfEq
            ( Literal 2 ) "temp" []
        )
    )
, Micro
    ( Code
        ( Set 1 "room" )
    )
, Micro
    ( Code
        ( Msg
            [ "You"
            , "went"
            , "west"
            , "through"
            , "a"
            , "small"
            , "wooden"
            , "door."
            ]
        )
    )
, Micro ( Code End )
, Micro
    ( Code
        ( IfEq
            ( Literal 3 ) "temp" []
        )
    )
, Micro
    ( Code
        ( Set 2 "has_green" )
    )
, Micro
    ( Code
        ( Msg
            [ "You"
            , "unlocked"
            , "the"
            , "door"
            , "on"
            , "the"
            , "floor!"
            ]
        )
    )
, Micro ( Code End )
, Micro ( Code End )
, Micro ( Code Line )
, Micro ( Code End )
, Micro
    ( Code
        ( IfEq
            ( Literal 7 ) "room" []
        )
    )
, Micro ( Code Line )
, Micro
    ( Code
        ( Msg
            [ "You"
            , "are"
            , "in"
            , "a"
            , "dark"
            , "room."
            , "There"
            , "are"
            , "doors"
            , "to"
            , "the"
            , "west,"
            , "to"
            , "the"
            , "east"
            , "and"
            , "to"
            , "the"
            , "south."
            ]
        )
    )
, Micro ( Code Line )
, Micro
    ( Code
        ( Msg
            [ "1."
            , "Go"
            , "east"
            , "through"
            , "the"
            , "door."
            ]
        )
    )
, Micro ( Code Line )
, Micro
    ( Code
        ( Msg
            [ "2."
            , "Go"
            , "west"
            , "through"
            , "the"
            , "door."
            ]
        )
    )
, Micro ( Code Line )
, Micro
    ( Code
        ( Msg
            [ "3."
            , "Go"
            , "south"
            , "through"
            , "the"
            , "door."
            ]
        )
    )
, Micro ( Code Line )
, Micro
    ( Code
        ( Msg
            [ "0."
            , "Menu"
            ]
        )
    )
, Micro ( Code Line )
, Micro ( Code Line )
, Micro
    ( Code
        ( Read "temp" )
    )
, Micro
    ( Code
        ( Dec 48 "temp" )
    )
, Micro
    ( Code
        ( IfEq
            ( Literal 0 ) "temp" []
        )
    )
, Micro
    ( Code
        ( Copy "room" "oldroom" )
    )
, Micro
    ( Code
        ( Set 0 "room" )
    )
, Micro ( Code End )
, Micro
    ( Code
        ( IfEq
            ( Literal 1 ) "temp" []
        )
    )
, Micro
    ( Code
        ( IfEq
            ( Literal 0 ) "has_red" []
        )
    )
, Micro
    ( Code
        ( Msg
            [ "You"
            , "can't!"
            , "It's"
            , "locked!"
            ]
        )
    )
, Micro ( Code End )
, Micro
    ( Code
        ( IfEq
            ( Literal 1 ) "has_red" []
        )
    )
, Micro
    ( Code
        ( Msg
            [ "You"
            , "unlocked"
            , "the"
            , "door!"
            , "You"
            , "went"
            , "east."
            ]
        )
    )
, Micro
    ( Code
        ( Set 8 "room" )
    )
, Micro ( Code End )
, Micro ( Code End )
, Micro
    ( Code
        ( IfEq
            ( Literal 2 ) "temp" []
        )
    )
, Micro
    ( Code
        ( Set 6 "room" )
    )
, Micro
    ( Code
        ( Msg
            [ "You"
            , "went"
            , "west."
            ]
        )
    )
, Micro ( Code End )
, Micro
    ( Code
        ( IfEq
            ( Literal 3 ) "temp" []
        )
    )
, Micro
    ( Code
        ( Set 3 "room" )
    )
, Micro
    ( Code
        ( Msg
            [ "You"
            , "went"
            , "south."
            ]
        )
    )
, Micro ( Code End )
, Micro ( Code Line )
, Micro ( Code End )
, Micro
    ( Code
        ( IfEq
            ( Literal 8 ) "room" []
        )
    )
, Micro ( Code Line )
, Micro
    ( Code
        ( Msg
            [ "There"
            , "is"
            , "a"
            , "table"
            , "in"
            , "this"
            , "room."
            , "You"
            , "can"
            , "hear"
            , "whispers."
            , "There"
            , "is"
            , "a"
            , "door"
            , "to"
            , "the"
            , "west."
            ]
        )
    )
, Micro
    ( Code
        ( IfEq
            ( Literal 0 ) "has_sword" []
        )
    )
, Micro
    ( Code
        ( Msg
            [ "There"
            , "is"
            , "a"
            , "long"
            , "sword"
            , "on"
            , "a"
            , "table."
            ]
        )
    )
, Micro ( Code End )
, Micro ( Code Line )
, Micro
    ( Code
        ( Msg
            [ "1."
            , "Go"
            , "west"
            , "through"
            , "the"
            , "door."
            ]
        )
    )
, Micro ( Code Line )
, Micro
    ( Code
        ( Msg
            [ "2."
            , "Look"
            , "behind"
            , "you!"
            ]
        )
    )
, Micro ( Code Line )
, Micro
    ( Code
        ( IfEq
            ( Literal 0 ) "has_sword" []
        )
    )
, Micro
    ( Code
        ( Msg
            [ "3."
            , "Take"
            , "the"
            , "sword."
            ]
        )
    )
, Micro ( Code Line )
, Micro ( Code End )
, Micro
    ( Code
        ( Msg
            [ "0."
            , "Menu"
            ]
        )
    )
, Micro ( Code Line )
, Micro ( Code Line )
, Micro
    ( Code
        ( Read "temp" )
    )
, Micro
    ( Code
        ( Dec 48 "temp" )
    )
, Micro
    ( Code
        ( IfEq
            ( Literal 0 ) "temp" []
        )
    )
, Micro
    ( Code
        ( Copy "room" "oldroom" )
    )
, Micro
    ( Code
        ( Set 0 "room" )
    )
, Micro ( Code End )
, Micro
    ( Code
        ( IfEq
            ( Literal 1 ) "temp" []
        )
    )
, Micro
    ( Code
        ( Set 7 "room" )
    )
, Micro
    ( Code
        ( Msg
            [ "You"
            , "ran"
            , "out"
            , "of"
            , "this"
            , "room!"
            ]
        )
    )
, Micro ( Code End )
, Micro
    ( Code
        ( IfEq
            ( Literal 2 ) "temp" []
        )
    )
, Micro
    ( Code
        ( Msg
            [ "You"
            , "see"
            , "no"
            , "one."
            , "Don't"
            , "be"
            , "such"
            , "a"
            , "wimp..."
            ]
        )
    )
, Micro ( Code End )
, Micro
    ( Code
        ( IfEq
            ( Literal 3 ) "temp" []
        )
    )
, Micro
    ( Code
        ( IfEq
            ( Literal 0 ) "has_sword" []
        )
    )
, Micro
    ( Code
        ( Msg
            [ "You"
            , "take"
            , "the"
            , "sword!"
            , "It"
            , "glows"
            , "blue"
            , "in"
            , "your"
            , "hands!"
            ]
        )
    )
, Micro
    ( Code
        ( Set 1 "has_sword" )
    )
, Micro ( Code End )
, Micro ( Code End )
, Micro ( Code Line )
, Micro ( Code End )
, Micro
    ( Code
        ( IfEq
            ( Literal 9 ) "room" []
        )
    )
, Micro ( Code Line )
, Micro
    ( Code
        ( Msg
            [ "You"
            , "are"
            , "in"
            , "a"
            , "hallway."
            , "There"
            , "are"
            , "four"
            , "doors."
            , "One"
            , "in"
            , "each"
            , "direction."
            ]
        )
    )
, Micro ( Code Line )
, Micro
    ( Code
        ( Msg
            [ "1."
            , "Go"
            , "west."
            ]
        )
    )
, Micro ( Code Line )
, Micro
    ( Code
        ( Msg
            [ "2."
            , "Go"
            , "north."
            ]
        )
    )
, Micro ( Code Line )
, Micro
    ( Code
        ( Msg
            [ "3."
            , "Go"
            , "east."
            ]
        )
    )
, Micro ( Code Line )
, Micro
    ( Code
        ( Msg
            [ "4."
            , "Go"
            , "south."
            ]
        )
    )
, Micro ( Code Line )
, Micro
    ( Code
        ( Msg
            [ "0."
            , "Menu"
            ]
        )
    )
, Micro ( Code Line )
, Micro ( Code Line )
, Micro
    ( Code
        ( Read "temp" )
    )
, Micro
    ( Code
        ( Dec 48 "temp" )
    )
, Micro
    ( Code
        ( IfEq
            ( Literal 0 ) "temp" []
        )
    )
, Micro
    ( Code
        ( Copy "room" "oldroom" )
    )
, Micro
    ( Code
        ( Set 0 "room" )
    )
, Micro ( Code End )
, Micro
    ( Code
        ( IfEq
            ( Literal 1 ) "temp" []
        )
    )
, Micro
    ( Code
        ( Set 2 "room" )
    )
, Micro
    ( Code
        ( Msg
            [ "You"
            , "went"
            , "west."
            ]
        )
    )
, Micro ( Code End )
, Micro
    ( Code
        ( IfEq
            ( Literal 2 ) "temp" []
        )
    )
, Micro
    ( Code
        ( Msg
            [ "You"
            , "went"
            , "north."
            ]
        )
    )
, Micro
    ( Code
        ( Set 5 "room" )
    )
, Micro ( Code End )
, Micro
    ( Code
        ( IfEq
            ( Literal 3 ) "temp" []
        )
    )
, Micro
    ( Code
        ( Set 4 "room" )
    )
, Micro
    ( Code
        ( Msg
            [ "You"
            , "went"
            , "east."
            ]
        )
    )
, Micro ( Code End )
, Micro
    ( Code
        ( IfEq
            ( Literal 4 ) "temp" []
        )
    )
, Micro
    ( Code
        ( Set 1 "room" )
    )
, Micro
    ( Code
        ( Msg
            [ "You"
            , "went"
            , "south."
            ]
        )
    )
, Micro ( Code End )
, Micro ( Code Line )
, Micro ( Code End )
, Micro ( Code End )
, Micro
    ( Code
        ( Msg
            [ "Bye"
            , "Bye!"
            ]
        )
    )
, Micro ( Code Line )
]