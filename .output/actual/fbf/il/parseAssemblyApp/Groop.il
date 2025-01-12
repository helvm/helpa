[ Compiler
    ( LineBreaks 80 )
, Compiler
    ( Dim [ "room" ] )
, Compiler
    ( Dim [ "oldroom" ] )
, Compiler
    ( Dim [ "has_green" ] )
, Compiler
    ( Dim [ "has_sword" ] )
, Compiler
    ( Dim [ "has_red" ] )
, Compiler
    ( Dim [ "playing" ] )
, Compiler
    ( Dim [ "temp" ] )
, Code
    ( Set 1 "playing" )
, Code
    ( Set 1 "room" )
, Code Line
, Code Line
, Code Line
, Code
    ( Msg
        [ "Welcome"
        , "to"
        , "Groop."
        ]
    )
, Code Line
, Code
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
, Code Line
, Code
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
, Code Line
, Code
    ( UnEq
        ( Literal 0 ) "playing" []
    )
, Code
    ( IfEq
        ( Literal 0 ) "room" []
    )
, Code Line
, Code
    ( Msg
        [ "***"
        , "MENU"
        , "***"
        ]
    )
, Code Line
, Code
    ( Msg
        [ "1."
        , "Exit"
        , "Game"
        ]
    )
, Code Line
, Code
    ( Msg
        [ "2."
        , "Continue"
        , "Game"
        ]
    )
, Code Line
, Code
    ( IfNotEq
        ( Literal 0 ) "has_red" []
    )
, Code
    ( Msg
        [ "You"
        , "have"
        , "a"
        , "red"
        , "key."
        ]
    )
, Code Line
, Code End
, Code
    ( IfNotEq
        ( Literal 0 ) "has_green" []
    )
, Code
    ( Msg
        [ "You"
        , "have"
        , "a"
        , "green"
        , "key."
        ]
    )
, Code Line
, Code End
, Code
    ( IfNotEq
        ( Literal 0 ) "has_sword" []
    )
, Code
    ( Msg
        [ "You"
        , "have"
        , "a"
        , "sword."
        ]
    )
, Code Line
, Code End
, Code
    ( Read "temp" )
, Code
    ( Dec 48 "temp" )
, Code
    ( IfEq
        ( Literal 1 ) "temp" []
    )
, Code
    ( Set 0 "playing" )
, Code End
, Code
    ( IfEq
        ( Literal 2 ) "temp" []
    )
, Code
    ( Copy "oldroom" "room" )
, Code End
, Code
    ( IfEq
        ( Literal 8 ) "temp" []
    )
, Code
    ( Read "room" )
, Code
    ( Dec 48 "room" )
, Code End
, Code End
, Code
    ( IfEq
        ( Literal 1 ) "room" []
    )
, Code Line
, Code
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
, Code Line
, Code
    ( Msg
        [ "1."
        , "Go"
        , "north"
        , "through"
        , "the"
        , "door."
        ]
    )
, Code Line
, Code
    ( Msg
        [ "2."
        , "Go"
        , "east"
        , "through"
        , "the"
        , "door."
        ]
    )
, Code Line
, Code
    ( Msg
        [ "0."
        , "Menu"
        ]
    )
, Code Line
, Code Line
, Code
    ( Read "temp" )
, Code
    ( Dec 48 "temp" )
, Code
    ( IfEq
        ( Literal 0 ) "temp" []
    )
, Code
    ( Copy "room" "oldroom" )
, Code
    ( Set 0 "room" )
, Code End
, Code
    ( IfEq
        ( Literal 1 ) "temp" []
    )
, Code
    ( Set 9 "room" )
, Code
    ( Msg
        [ "You"
        , "went"
        , "north."
        ]
    )
, Code End
, Code
    ( IfEq
        ( Literal 2 ) "temp" []
    )
, Code
    ( Set 6 "room" )
, Code
    ( Msg
        [ "You"
        , "went"
        , "east."
        ]
    )
, Code End
, Code Line
, Code End
, Code
    ( IfEq
        ( Literal 2 ) "room" []
    )
, Code Line
, Code
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
, Code Line
, Code
    ( Msg
        [ "1."
        , "Open"
        , "the"
        , "closet."
        ]
    )
, Code Line
, Code
    ( Msg
        [ "2."
        , "Go"
        , "east"
        , "through"
        , "the"
        , "door."
        ]
    )
, Code Line
, Code
    ( Msg
        [ "0."
        , "Menu"
        ]
    )
, Code Line
, Code Line
, Code
    ( Read "temp" )
, Code
    ( Dec 48 "temp" )
, Code
    ( IfEq
        ( Literal 0 ) "temp" []
    )
, Code
    ( Copy "room" "oldroom" )
, Code
    ( Set 0 "room" )
, Code End
, Code
    ( IfEq
        ( Literal 1 ) "temp" []
    )
, Code
    ( IfEq
        ( Literal 1 ) "has_green" []
    )
, Code
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
, Code End
, Code
    ( IfEq
        ( Literal 0 ) "has_green" []
    )
, Code
    ( Set 1 "has_green" )
, Code
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
, Code End
, Code End
, Code
    ( IfEq
        ( Literal 2 ) "temp" []
    )
, Code
    ( Set 9 "room" )
, Code
    ( Msg
        [ "You"
        , "went"
        , "east."
        ]
    )
, Code End
, Code Line
, Code End
, Code
    ( IfEq
        ( Literal 3 ) "room" []
    )
, Code Line
, Code
    ( IfEq
        ( Literal 1 ) "has_red" []
    )
, Code
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
, Code Line
, Code
    ( Msg
        [ "1."
        , "Go"
        , "north"
        , "through"
        , "the"
        , "door."
        ]
    )
, Code Line
, Code
    ( Msg
        [ "0."
        , "Menu"
        ]
    )
, Code Line
, Code Line
, Code
    ( Read "temp" )
, Code
    ( Dec 48 "temp" )
, Code
    ( IfEq
        ( Literal 0 ) "temp" []
    )
, Code
    ( Copy "room" "oldroom" )
, Code
    ( Set 0 "room" )
, Code End
, Code
    ( IfEq
        ( Literal 1 ) "temp" []
    )
, Code
    ( Set 7 "room" )
, Code
    ( Msg
        [ "You"
        , "went"
        , "north."
        ]
    )
, Code End
, Code End
, Code
    ( IfEq
        ( Literal 0 ) "has_red" []
    )
, Code
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
, Code Line
, Code
    ( Msg
        [ "1."
        , "Go"
        , "north"
        , "through"
        , "the"
        , "door."
        ]
    )
, Code Line
, Code
    ( Msg
        [ "2."
        , "Break"
        , "the"
        , "bottle."
        ]
    )
, Code Line
, Code
    ( Msg
        [ "0."
        , "Menu"
        ]
    )
, Code Line
, Code Line
, Code
    ( Read "temp" )
, Code
    ( Dec 48 "temp" )
, Code
    ( IfEq
        ( Literal 0 ) "temp" []
    )
, Code
    ( Copy "room" "oldroom" )
, Code
    ( Set 0 "room" )
, Code End
, Code
    ( IfEq
        ( Literal 1 ) "temp" []
    )
, Code
    ( Set 7 "room" )
, Code
    ( Msg
        [ "You"
        , "went"
        , "north."
        ]
    )
, Code End
, Code
    ( IfEq
        ( Literal 2 ) "temp" []
    )
, Code
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
, Code
    ( Set 1 "has_red" )
, Code End
, Code End
, Code Line
, Code End
, Code
    ( IfEq
        ( Literal 4 ) "room" []
    )
, Code Line
, Code
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
, Code Line
, Code
    ( Msg
        [ "1."
        , "Go"
        , "east"
        , "through"
        , "the"
        , "door."
        ]
    )
, Code Line
, Code
    ( Msg
        [ "2."
        , "Go"
        , "west"
        , "through"
        , "the"
        , "door."
        ]
    )
, Code Line
, Code
    ( Msg
        [ "0."
        , "Menu"
        ]
    )
, Code Line
, Code Line
, Code
    ( Read "temp" )
, Code
    ( Dec 48 "temp" )
, Code
    ( IfEq
        ( Literal 0 ) "temp" []
    )
, Code
    ( Copy "room" "oldroom" )
, Code
    ( Set 0 "room" )
, Code End
, Code
    ( IfEq
        ( Literal 1 ) "temp" []
    )
, Code
    ( Set 6 "room" )
, Code
    ( Msg
        [ "You"
        , "went"
        , "east."
        ]
    )
, Code End
, Code
    ( IfEq
        ( Literal 2 ) "temp" []
    )
, Code
    ( Set 9 "room" )
, Code
    ( Msg
        [ "You"
        , "went"
        , "west."
        ]
    )
, Code End
, Code Line
, Code End
, Code
    ( IfEq
        ( Literal 5 ) "room" []
    )
, Code Line
, Code
    ( IfEq
        ( Literal 0 ) "has_sword" []
    )
, Code
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
, Code End
, Code
    ( IfEq
        ( Literal 1 ) "has_sword" []
    )
, Code
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
, Code End
, Code Line
, Code
    ( Set 0 "playing" )
, Code End
, Code
    ( IfEq
        ( Literal 6 ) "room" []
    )
, Code Line
, Code
    ( IfEq
        ( Literal 0 ) "has_green" []
    )
, Code
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
, Code Line
, Code
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
, Code Line
, Code
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
, Code Line
, Code
    ( Msg
        [ "0."
        , "Menu"
        ]
    )
, Code Line
, Code Line
, Code
    ( Read "temp" )
, Code
    ( Dec 48 "temp" )
, Code
    ( IfEq
        ( Literal 0 ) "temp" []
    )
, Code
    ( Copy "room" "oldroom" )
, Code
    ( Set 0 "room" )
, Code End
, Code
    ( IfEq
        ( Literal 1 ) "temp" []
    )
, Code
    ( Set 4 "room" )
, Code
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
, Code End
, Code
    ( IfEq
        ( Literal 2 ) "temp" []
    )
, Code
    ( Set 1 "room" )
, Code
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
, Code End
, Code End
, Code
    ( IfEq
        ( Literal 2 ) "has_green" []
    )
, Code
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
, Code Line
, Code
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
, Code Line
, Code
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
, Code Line
, Code
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
, Code Line
, Code
    ( Msg
        [ "0."
        , "Menu"
        ]
    )
, Code Line
, Code Line
, Code
    ( Read "temp" )
, Code
    ( Dec 48 "temp" )
, Code
    ( IfEq
        ( Literal 0 ) "temp" []
    )
, Code
    ( Copy "room" "oldroom" )
, Code
    ( Set 0 "room" )
, Code End
, Code
    ( IfEq
        ( Literal 1 ) "temp" []
    )
, Code
    ( Set 4 "room" )
, Code
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
, Code End
, Code
    ( IfEq
        ( Literal 2 ) "temp" []
    )
, Code
    ( Set 1 "room" )
, Code
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
, Code End
, Code
    ( IfEq
        ( Literal 3 ) "temp" []
    )
, Code
    ( Set 7 "room" )
, Code
    ( Msg
        [ "You"
        , "went"
        , "down."
        ]
    )
, Code End
, Code End
, Code
    ( IfEq
        ( Literal 1 ) "has_green" []
    )
, Code
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
, Code Line
, Code
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
, Code Line
, Code
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
, Code Line
, Code
    ( Msg
        [ "3."
        , "Unlock"
        , "the"
        , "floor"
        , "door."
        ]
    )
, Code Line
, Code
    ( Msg
        [ "0."
        , "Menu"
        ]
    )
, Code Line
, Code Line
, Code
    ( Read "temp" )
, Code
    ( Dec 48 "temp" )
, Code
    ( IfEq
        ( Literal 0 ) "temp" []
    )
, Code
    ( Copy "room" "oldroom" )
, Code
    ( Set 0 "room" )
, Code End
, Code
    ( IfEq
        ( Literal 1 ) "temp" []
    )
, Code
    ( Set 4 "room" )
, Code
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
, Code End
, Code
    ( IfEq
        ( Literal 2 ) "temp" []
    )
, Code
    ( Set 1 "room" )
, Code
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
, Code End
, Code
    ( IfEq
        ( Literal 3 ) "temp" []
    )
, Code
    ( Set 2 "has_green" )
, Code
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
, Code End
, Code End
, Code Line
, Code End
, Code
    ( IfEq
        ( Literal 7 ) "room" []
    )
, Code Line
, Code
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
, Code Line
, Code
    ( Msg
        [ "1."
        , "Go"
        , "east"
        , "through"
        , "the"
        , "door."
        ]
    )
, Code Line
, Code
    ( Msg
        [ "2."
        , "Go"
        , "west"
        , "through"
        , "the"
        , "door."
        ]
    )
, Code Line
, Code
    ( Msg
        [ "3."
        , "Go"
        , "south"
        , "through"
        , "the"
        , "door."
        ]
    )
, Code Line
, Code
    ( Msg
        [ "0."
        , "Menu"
        ]
    )
, Code Line
, Code Line
, Code
    ( Read "temp" )
, Code
    ( Dec 48 "temp" )
, Code
    ( IfEq
        ( Literal 0 ) "temp" []
    )
, Code
    ( Copy "room" "oldroom" )
, Code
    ( Set 0 "room" )
, Code End
, Code
    ( IfEq
        ( Literal 1 ) "temp" []
    )
, Code
    ( IfEq
        ( Literal 0 ) "has_red" []
    )
, Code
    ( Msg
        [ "You"
        , "can't!"
        , "It's"
        , "locked!"
        ]
    )
, Code End
, Code
    ( IfEq
        ( Literal 1 ) "has_red" []
    )
, Code
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
, Code
    ( Set 8 "room" )
, Code End
, Code End
, Code
    ( IfEq
        ( Literal 2 ) "temp" []
    )
, Code
    ( Set 6 "room" )
, Code
    ( Msg
        [ "You"
        , "went"
        , "west."
        ]
    )
, Code End
, Code
    ( IfEq
        ( Literal 3 ) "temp" []
    )
, Code
    ( Set 3 "room" )
, Code
    ( Msg
        [ "You"
        , "went"
        , "south."
        ]
    )
, Code End
, Code Line
, Code End
, Code
    ( IfEq
        ( Literal 8 ) "room" []
    )
, Code Line
, Code
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
, Code
    ( IfEq
        ( Literal 0 ) "has_sword" []
    )
, Code
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
, Code End
, Code Line
, Code
    ( Msg
        [ "1."
        , "Go"
        , "west"
        , "through"
        , "the"
        , "door."
        ]
    )
, Code Line
, Code
    ( Msg
        [ "2."
        , "Look"
        , "behind"
        , "you!"
        ]
    )
, Code Line
, Code
    ( IfEq
        ( Literal 0 ) "has_sword" []
    )
, Code
    ( Msg
        [ "3."
        , "Take"
        , "the"
        , "sword."
        ]
    )
, Code Line
, Code End
, Code
    ( Msg
        [ "0."
        , "Menu"
        ]
    )
, Code Line
, Code Line
, Code
    ( Read "temp" )
, Code
    ( Dec 48 "temp" )
, Code
    ( IfEq
        ( Literal 0 ) "temp" []
    )
, Code
    ( Copy "room" "oldroom" )
, Code
    ( Set 0 "room" )
, Code End
, Code
    ( IfEq
        ( Literal 1 ) "temp" []
    )
, Code
    ( Set 7 "room" )
, Code
    ( Msg
        [ "You"
        , "ran"
        , "out"
        , "of"
        , "this"
        , "room!"
        ]
    )
, Code End
, Code
    ( IfEq
        ( Literal 2 ) "temp" []
    )
, Code
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
, Code End
, Code
    ( IfEq
        ( Literal 3 ) "temp" []
    )
, Code
    ( IfEq
        ( Literal 0 ) "has_sword" []
    )
, Code
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
, Code
    ( Set 1 "has_sword" )
, Code End
, Code End
, Code Line
, Code End
, Code
    ( IfEq
        ( Literal 9 ) "room" []
    )
, Code Line
, Code
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
, Code Line
, Code
    ( Msg
        [ "1."
        , "Go"
        , "west."
        ]
    )
, Code Line
, Code
    ( Msg
        [ "2."
        , "Go"
        , "north."
        ]
    )
, Code Line
, Code
    ( Msg
        [ "3."
        , "Go"
        , "east."
        ]
    )
, Code Line
, Code
    ( Msg
        [ "4."
        , "Go"
        , "south."
        ]
    )
, Code Line
, Code
    ( Msg
        [ "0."
        , "Menu"
        ]
    )
, Code Line
, Code Line
, Code
    ( Read "temp" )
, Code
    ( Dec 48 "temp" )
, Code
    ( IfEq
        ( Literal 0 ) "temp" []
    )
, Code
    ( Copy "room" "oldroom" )
, Code
    ( Set 0 "room" )
, Code End
, Code
    ( IfEq
        ( Literal 1 ) "temp" []
    )
, Code
    ( Set 2 "room" )
, Code
    ( Msg
        [ "You"
        , "went"
        , "west."
        ]
    )
, Code End
, Code
    ( IfEq
        ( Literal 2 ) "temp" []
    )
, Code
    ( Msg
        [ "You"
        , "went"
        , "north."
        ]
    )
, Code
    ( Set 5 "room" )
, Code End
, Code
    ( IfEq
        ( Literal 3 ) "temp" []
    )
, Code
    ( Set 4 "room" )
, Code
    ( Msg
        [ "You"
        , "went"
        , "east."
        ]
    )
, Code End
, Code
    ( IfEq
        ( Literal 4 ) "temp" []
    )
, Code
    ( Set 1 "room" )
, Code
    ( Msg
        [ "You"
        , "went"
        , "south."
        ]
    )
, Code End
, Code Line
, Code End
, Code End
, Code
    ( Msg
        [ "Bye"
        , "Bye!"
        ]
    )
, Code Line
]