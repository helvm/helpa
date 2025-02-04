[ Directive
    ( PushNatural 1 )
, Directive
    ( Label
        ( LNatural 2 )
    )
, Command Dup
, Directive
    ( PushNatural 15 )
, Command Mod
, Directive
    ( Branch ( Just BZ )
        ( BLIdentifier "_track_3" )
    )
, Command Dup
, Directive
    ( PushNatural 3 )
, Command Mod
, Directive
    ( Branch ( Just BZ )
        ( BLIdentifier "_track_1" )
    )
, Command Dup
, Directive
    ( PushNatural 5 )
, Command Mod
, Directive
    ( Branch ( Just BZ )
        ( BLIdentifier "_track_2" )
    )
, Command Dup
, Command OutN
, Directive
    ( PushNatural 10 )
, Command Out
, Directive
    ( Label
        ( LIdentifier "_track_0" )
    )
, Directive
    ( PushNatural 1 )
, Command Add
, Command Dup
, Directive
    ( PushNatural 100 )
, Directive
    ( Branch ( Just BLE )
        ( BLNatural ( Just Backward ) 2 )
    )
, Directive Halt
, Directive Track
, Directive
    ( Print "Fizz
    " )
, Directive
    ( Branch Nothing
        ( BLIdentifier "_track_0" )
    )
, Directive Track
, Directive
    ( Print "Buzz
    " )
, Directive
    ( Branch Nothing
        ( BLIdentifier "_track_0" )
    )
, Directive Track
, Directive
    ( Print "FizzBuzz
    " )
, Directive
    ( Branch Nothing
        ( BLIdentifier "_track_0" )
    )
]