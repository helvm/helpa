[ Micro ( Directive Track )
, Micro ( Command Pop )
, Micro
    ( Directive
        ( PushInteger 1 )
    )
, Micro
    ( Directive
        ( PushInteger 2 )
    )
, Micro
    ( Directive
        ( PushInteger 1 )
    )
, Micro ( Command Roll )
, Micro ( Command Dup )
, Micro
    ( Directive
        ( PushInteger 3 )
    )
, Micro
    ( Directive
        ( PushInteger 1 )
    )
, Micro ( Command Roll )
, Micro ( Command Dup )
, Micro
    ( Directive
        ( PushInteger 2 )
    )
, Micro ( Command Mod )
, Micro
    ( Directive
        ( Branch Nothing ( Just BNZ )
            ( BLNatural ( Just Forward ) 1 )
        )
    )
, Micro
    ( Directive
        ( PushInteger 1 )
    )
, Micro
    ( Directive
        ( PushInteger 2 )
    )
, Micro
    ( Directive
        ( PushInteger 1 )
    )
, Micro ( Command Roll )
, Micro
    ( Directive
        ( PushString "2 - rip off the bedclothes
        " )
    )
, Micro ( Command Dup )
, Micro
    ( Directive
        ( PushInteger 2 )
    )
, Micro ( Command Add )
, Micro
    ( Directive
        ( PushInteger
            ( -1 )
        )
    )
, Micro ( Command Roll )
, Micro
    ( Directive
        ( Label
            ( LNatural 1 )
        )
    )
, Micro
    ( Directive
        ( PushInteger 1 )
    )
, Micro
    ( Directive
        ( PushInteger 2 )
    )
, Micro
    ( Directive
        ( PushInteger 1 )
    )
, Micro ( Command Roll )
, Micro
    ( Directive
        ( PushString "
        Your options:
        1 - return to the hallway
        " )
    )
, Micro ( Command Dup )
, Micro
    ( Directive
        ( PushInteger 2 )
    )
, Micro ( Command Add )
, Micro
    ( Directive
        ( PushInteger
            ( -1 )
        )
    )
, Micro ( Command Roll )
, Micro
    ( Directive
        ( PushInteger 2 )
    )
, Micro ( Command Mod )
, Micro
    ( Directive
        ( Branch Nothing ( Just BNZ )
            ( BLNatural ( Just Forward ) 1 )
        )
    )
, Micro
    ( Directive
        ( PushInteger 1 )
    )
, Micro
    ( Directive
        ( PushString "Sunbeams warm the carefully made bed.
        " )
    )
, Micro
    ( Directive
        ( Branch Nothing Nothing
            ( BLNatural ( Just Forward ) 0 )
        )
    )
, Micro
    ( Directive
        ( Label
            ( LNatural 1 )
        )
    )
, Micro
    ( Directive
        ( PushInteger 1 )
    )
, Micro
    ( Directive
        ( PushString "The harsh sunlight exposes the unmade bed.
        You feel a little guilty.
        " )
    )
, Micro
    ( Directive
        ( Label
            ( LNatural 0 )
        )
    )
, Micro
    ( Directive
        ( PushInteger 1 )
    )
, Micro
    ( Directive
        ( PushString "a cozy bedroom. " )
    )
, Micro
    ( Directive
        ( Label
            ( LNatural 4 )
        )
    )
, Call [] "print"
, Micro
    ( Directive
        ( Branch Nothing ( Just BNZ )
            ( BLNatural ( Just Backward ) 4 )
        )
    )
, Call [] "scan"
, Micro
    ( Directive
        ( PushInteger 1 )
    )
, Micro ( Command Sub )
, Micro
    ( Directive
        ( BranchTable
            [ BLIdentifier "_track_0"
            , BLNatural ( Just Forward ) 1
            ]
        )
    )
, Micro
    ( Directive
        ( Label
            ( LNatural 3 )
        )
    )
, Micro
    ( Directive
        ( PushInteger 0 )
    )
, Micro
    ( Directive
        ( PushString "I beg your pardon?
        " )
    )
, Micro
    ( Directive
        ( Branch Nothing Nothing
            ( BLNatural ( Just Backward ) 4 )
        )
    )
, Micro
    ( Directive
        ( Label
            ( LNatural 1 )
        )
    )
, Micro
    ( Directive
        ( PushInteger 2 )
    )
, Micro
    ( Directive
        ( PushInteger 1 )
    )
, Micro ( Command Roll )
, Micro ( Command Dup )
, Micro
    ( Directive
        ( PushInteger 3 )
    )
, Micro
    ( Directive
        ( PushInteger 1 )
    )
, Micro ( Command Roll )
, Micro
    ( Directive
        ( PushInteger 2 )
    )
, Micro ( Command Mod )
, Micro
    ( Directive
        ( Branch Nothing ( Just BNZ )
            ( BLNatural ( Just Backward ) 3 )
        )
    )
, Micro ( Command Pop )
, Micro
    ( Directive
        ( PushInteger 5 )
    )
, Micro
    ( Directive
        ( Branch Nothing Nothing
            ( BLIdentifier "_track_0" )
        )
    )
]