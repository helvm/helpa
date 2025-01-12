[ Micro
    ( Directive
        ( PushInteger 1 )
    )
, Micro
    ( Directive
        ( Label
            ( Literal 2 )
        )
    )
, Micro ( Command Dup )
, Micro
    ( Directive
        ( PushInteger 15 )
    )
, Micro ( Command Mod )
, Micro
    ( Directive
        ( Branch Nothing ( Just BZ )
            ( BLIdentifier "_track_3" )
        )
    )
, Micro ( Command Dup )
, Micro
    ( Directive
        ( PushInteger 3 )
    )
, Micro ( Command Mod )
, Micro
    ( Directive
        ( Branch Nothing ( Just BZ )
            ( BLIdentifier "_track_1" )
        )
    )
, Micro ( Command Dup )
, Micro
    ( Directive
        ( PushInteger 5 )
    )
, Micro ( Command Mod )
, Micro
    ( Directive
        ( Branch Nothing ( Just BZ )
            ( BLIdentifier "_track_2" )
        )
    )
, Micro ( Command Dup )
, Micro ( Command OutN )
, Micro
    ( Directive
        ( PushInteger 10 )
    )
, Micro ( Command Out )
, Micro
    ( Directive
        ( Label
            ( Variable "_track_0" )
        )
    )
, Micro
    ( Directive
        ( PushInteger 1 )
    )
, Micro ( Command Add )
, Micro ( Command Dup )
, Micro
    ( Directive
        ( PushInteger 100 )
    )
, Micro
    ( Directive
        ( Branch Nothing ( Just BLE )
            ( BLNatural ( Just Backward ) 2 )
        )
    )
, Micro ( Directive Halt )
, Micro ( Directive Track )
, Micro
    ( Directive
        ( Print "Fizz
        " )
    )
, Micro
    ( Directive
        ( Branch Nothing Nothing
            ( BLIdentifier "_track_0" )
        )
    )
, Micro ( Directive Track )
, Micro
    ( Directive
        ( Print "Buzz
        " )
    )
, Micro
    ( Directive
        ( Branch Nothing Nothing
            ( BLIdentifier "_track_0" )
        )
    )
, Micro ( Directive Track )
, Micro
    ( Directive
        ( Print "FizzBuzz
        " )
    )
, Micro
    ( Directive
        ( Branch Nothing Nothing
            ( BLIdentifier "_track_0" )
        )
    )
]